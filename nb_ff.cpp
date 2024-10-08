/*----------------------------------------------------------------------------/
 /  FatFs - Generic FAT file system module  R0.12c                             /
 /-----------------------------------------------------------------------------/
 /
 / Copyright (C) 2017, ChaN, all right reserved.
 /
 / FatFs module is an open source software. Redistribution and use of FatFs in
 / source and binary forms, with or without modification, are permitted provided
 / that the following condition is met:
 /
 / 1. Redistributions of source code must retain the above copyright notice,
 /    this condition and the following disclaimer.
 /
 / This software is provided by the copyright holder and contributors "AS IS"
 / and any warranties related to this software are DISCLAIMED.
 / The copyright owner or contributors be NOT LIABLE for any damages caused
 / by use of this software.
 /----------------------------------------------------------------------------*/

// #include "nb_ff.h"			/* Declarations of FatFs API */ // Included with Defs
// #include "nb_diskio.h"		/* Declarations of device I/O functions */ // Included with Defs
#include "nb_ff_Defs.h"

namespace FatFS_NB {

	/*--------------------------------------------------------------------------

	 Module Private Definitions

	 ---------------------------------------------------------------------------*/

#if _NB_FATFS != 68300 /* Revision ID */
#error Wrong include file (ff.h).
#endif

	/*--------------------------------------------------------------------------

	 Module Private Work Area

	 ---------------------------------------------------------------------------*/

	/* Remark: Variables defined here without initial value shall be guaranteed
	 /  zero/null at start-up. If not, the linker option or start-up routine is
	 /  not compliance with C standard. */

#if _NB_VOLUMES < 1 || _NB_VOLUMES > 10
#error Wrong _NB_VOLUMES setting
#endif
	static FATFS *FatFs[_NB_VOLUMES]; /* Pointer to the file system objects (logical drives) */
	static WORD Fsid; /* File system mount ID */

#if _NB_FS_RPATH != 0 && _NB_VOLUMES >= 2
	static BYTE CurrVol; /* Current drive */
#endif

#if _NB_FS_LOCK != 0
	static FILESEM Files[_NB_FS_LOCK]; /* Open object lock semaphores */
#endif

#if _NB_USE_LFN == 0 /* Non-LFN configuration */
#define DEF_NAMBUF
#define INIT_NAMBUF(fs)
#define FREE_NAMBUF()

#else /* LFN configuration */
#if _NB_MAX_LFN < 12 || _NB_MAX_LFN > 255
#error Wrong _MAX_LFN value
#endif
#define MAXDIRB(nc) ((nc + 44U) / 15 * SZDIRE)

#if _NB_USE_LFN == 1 /* LFN enabled with static working buffer */
#if _NB_FS_EXFAT
	static BYTE DirBuf[MAXDIRB(_NB_MAX_LFN)]; /* Directory entry block scratchpad buffer */
#endif
	static WCHAR LfnBuf[_NB_MAX_LFN + 1]; /* LFN enabled with static working buffer */
#define DEF_NAMBUF
#define INIT_NAMBUF(fs)
#define FREE_NAMBUF()

#elif _NB_USE_LFN == 2 /* LFN enabled with dynamic working buffer on the stack */
#if _NB_FS_EXFAT
#define DEF_NAMBUF            \
	WCHAR lbuf[_MAX_LFN + 1]; \
	BYTE dbuf[MAXDIRB(_MAX_LFN)];
#define INIT_NAMBUF(fs)         \
	{                           \
		(fs)->lfnBuffer = lbuf; \
		(fs)->dirBuffer = dbuf; \
	}
#define FREE_NAMBUF()
#else
#define DEF_NAMBUF WCHAR lbuf[_MAX_LFN + 1];
#define INIT_NAMBUF(fs)         \
	{                           \
		(fs)->lfnBuffer = lbuf; \
	}
#define FREE_NAMBUF()
#endif

#elif _NB_USE_LFN == 3 /* LFN enabled with dynamic working buffer on the heap */
#if _NB_FS_EXFAT
#define DEF_NAMBUF WCHAR *lfn;
#define INIT_NAMBUF(fs)                                            \
	{                                                              \
		lfn = ff_memalloc((_MAX_LFN + 1) * 2 + MAXDIRB(_MAX_LFN)); \
		if (!lfn)                                                  \
			LEAVE_FF(fs, FR_NOT_ENOUGH_CORE);                      \
		(fs)->lfnBuffer = lfn;                                     \
		(fs)->dirBuffer = (BYTE *)(lfn + _MAX_LFN + 1);            \
	}
#define FREE_NAMBUF() ff_memfree(lfn)
#else
#define DEF_NAMBUF WCHAR *lfn;
#define INIT_NAMBUF(fs)                        \
	{                                          \
		lfn = ff_memalloc((_MAX_LFN + 1) * 2); \
		if (!lfn)                              \
			LEAVE_FF(fs, FR_NOT_ENOUGH_CORE);  \
		(fs)->lfnBuffer = lfn;                 \
	}
#define FREE_NAMBUF() ff_memfree(lfn)
#endif

#else
#error Wrong _USE_LFN setting

#endif
#endif /* else _USE_LFN == 0 */

#ifdef _EXCVT
	static const BYTE ExCvt[] = _EXCVT; /* Upper conversion table for SBCS extended characters */
#endif

	static void *voidPtr;

	static DWORD getFatValue;
	static DWORD findBitmapValue;
	static DWORD createChainValue;
	static BYTE checkFS_Value;
	static int putc_flush_Value;

	static FRESULT fResult = FR_NOT_READY;

	const uint8_t maxCallbacks = 10;
	static volatile uint8_t callbackCounter = 0;

	static void (*callbackFunctions[maxCallbacks])(void*);
	static void *voidDatasets[maxCallbacks];

	static volatile bool pollingMode = false;
	static volatile bool pollCallReady = false;
	static volatile bool busy = false;

	static void addCallback(void (*callback)(void*), void *data) {
		busy = true;
		if (callbackCounter >= maxCallbacks) {
			// Error 
		} else {
			callbackFunctions[callbackCounter] = callback;
			voidDatasets[callbackCounter] = data;
			callbackCounter = callbackCounter + 1;
		}
	}

	static void callNextCallback() {
		if(!pollingMode){
			if (callbackCounter > 0 && callbackCounter < 11) {
				callbackCounter = callbackCounter - 1;
				callbackFunctions[callbackCounter](voidDatasets[callbackCounter]);
				if(callbackCounter == 0){
					busy = false;
				}
			} else {
				// Error 
			}
		}else{
			pollCallReady = true;
		}
	}

	void setPollingMode(bool mode){
		pollingMode = mode;
	}
	
	bool getPollingCallReady(){
		return pollCallReady;
	}

	void pollingModeCall(){
		if(busy && pollCallReady){
			if (callbackCounter > 0 && callbackCounter < 11) {
				pollCallReady = false;
				callbackCounter = callbackCounter - 1;
				callbackFunctions[callbackCounter](voidDatasets[callbackCounter]);
				if(callbackCounter == 0){
					busy = false;
				}
			} else {
				// Error 
			}
		}
	}

	bool getBusy(){
		return busy;
	}

	/*--------------------------------------------------------------------------

	 Module Private Functions

	 ---------------------------------------------------------------------------*/

	/*-----------------------------------------------------------------------*/
	/* Load/Store multi-byte word in the FAT structure                       */
	/*-----------------------------------------------------------------------*/

	static WORD ld_word(const BYTE *ptr) /*	 Load a 2-byte little-endian word */
	{
		WORD rv;

		rv = ptr[1];
		rv = rv << 8 | ptr[0];
		return rv;
	}

	static DWORD ld_dword(const BYTE *ptr) /* Load a 4-byte little-endian word */
	{
		DWORD rv;

		rv = ptr[3];
		rv = rv << 8 | ptr[2];
		rv = rv << 8 | ptr[1];
		rv = rv << 8 | ptr[0];
		return rv;
	}

#if _NB_FS_EXFAT
	static QWORD ld_qword(const BYTE *ptr) /* Load an 8-byte little-endian word */
	{
		QWORD rv;

		rv = ptr[7];
		rv = rv << 8 | ptr[6];
		rv = rv << 8 | ptr[5];
		rv = rv << 8 | ptr[4];
		rv = rv << 8 | ptr[3];
		rv = rv << 8 | ptr[2];
		rv = rv << 8 | ptr[1];
		rv = rv << 8 | ptr[0];
		return rv;
	}
#endif

#if !_NB_FS_READONLY
	static void st_word(BYTE *ptr, WORD val) /* Store a 2-byte word in little-endian */
	{
		*ptr++ = (BYTE) val;
		val >>= 8;
		*ptr++ = (BYTE) val;
	}

	static void st_dword(BYTE *ptr, DWORD val) /* Store a 4-byte word in little-endian */
	{
		*ptr++ = (BYTE) val;
		val >>= 8;
		*ptr++ = (BYTE) val;
		val >>= 8;
		*ptr++ = (BYTE) val;
		val >>= 8;
		*ptr++ = (BYTE) val;
	}

#if _NB_FS_EXFAT
	static void st_qword(BYTE *ptr, QWORD val) /* Store an 8-byte word in little-endian */
	{
		*ptr++ = (BYTE) val;
		val >>= 8;
		*ptr++ = (BYTE) val;
		val >>= 8;
		*ptr++ = (BYTE) val;
		val >>= 8;
		*ptr++ = (BYTE) val;
		val >>= 8;
		*ptr++ = (BYTE) val;
		val >>= 8;
		*ptr++ = (BYTE) val;
		val >>= 8;
		*ptr++ = (BYTE) val;
		val >>= 8;
		*ptr++ = (BYTE) val;
	}
#endif
#endif /* !_FS_READONLY */

	/*-----------------------------------------------------------------------*/
	/* String functions                                                      */
	/*-----------------------------------------------------------------------*/

	/* Copy memory to memory */
	static void mem_cpy(void *dst, const void *src, UINT cnt) {
		BYTE *d = (BYTE*) dst;
		const BYTE *s = (const BYTE*) src;

		if (cnt) {
			do {
				*d++ = *s++;
			} while (--cnt);
		}
	}

	/* Fill memory block */
	static void mem_set(void *dst, int val, UINT cnt) {
		BYTE *d = (BYTE*) dst;

		do {
			*d++ = (BYTE) val;
		} while (--cnt);
	}

	/* Compare memory block */
	static int mem_cmp(const void *dst, const void *src, UINT cnt) { /* ZR:same, NZ:different */
		const BYTE *d = (const BYTE*) dst, *s = (const BYTE*) src;
		int r = 0;

		do {
			r = *d++ - *s++;
		} while (--cnt && r == 0);

		return r;
	}

	/* Check if chr is contained in the string */
	static int chk_chr(const char *str, int chr) { /* NZ:contained, ZR:not contained */
		while (*str && *str != chr)
			str++;
		return *str;
	}

#if _NB_FS_REENTRANT
#error
	/*-----------------------------------------------------------------------*/
	/* Request/Release grant to access the volume                            */
	/*-----------------------------------------------------------------------*/
	static int lock_fs(
		FATFS *fs /* File system object */
	)
	{
		return (fs && ff_req_grant(fs->sobj)) ? 1 : 0;
	}

	static void unlock_fs(
		FATFS *fs,	/* File system object */
		FRESULT res /* Result code to be returned */
	)
	{
		if (fs && res != FR_NOT_ENABLED && res != FR_INVALID_DRIVE && res != FR_TIMEOUT)
		{
			ff_rel_grant(fs->sobj);
		}
	}

#endif

#if _NB_FS_LOCK != 0
	/*-----------------------------------------------------------------------*/
	/* File lock control functions                                           */
	/*-----------------------------------------------------------------------*/

	static FRESULT chk_lock( /* Check if the file can be accessed */
	DIR *dp, /* Directory object pointing the file to be checked */
	int acc /* Desired access type (0:Read, 1:Write, 2:Delete/Rename) */
	) {
		UINT i, be;

		/* Search file semaphore table */
		for (i = be = 0; i < _NB_FS_LOCK; i++) {
			if (Files[i].fs) { /* Existing entry */
				if (Files[i].fs == dp->obj.fs && /* Check if the object matched with an open object */
				Files[i].clu == dp->obj.startCluster && Files[i].ofs == dp->currentReadWriteOffset)
					break;
			} else { /* Blank entry */
				be = 1;
			}
		}
		if (i == _NB_FS_LOCK) { /* The object is not opened */
			return (be || acc == 2) ? FR_OK : FR_TOO_MANY_OPEN_FILES; /* Is there a blank entry for new object? */
		}

		/* The object has been opened. Reject any open against writing file and all write mode open */
		return (acc || Files[i].ctr == 0x100) ? FR_LOCKED : FR_OK;
	}

	static int enq_lock(void) /* Check if an entry is available for a new object */
	{
		UINT i;

		for (i = 0; i < _NB_FS_LOCK && Files[i].fs; i++)
			;
		return (i == _NB_FS_LOCK) ? 0 : 1;
	}

	static UINT inc_lock( /* Increment object open counter and returns its index (0:Internal error) */
	DIR *dp, /* Directory object pointing the file to register or increment */
	int acc /* Desired access (0:Read, 1:Write, 2:Delete/Rename) */
	) {
		UINT i;

		for (i = 0; i < _NB_FS_LOCK; i++) { /* Find the object */
			if (Files[i].fs == dp->obj.fs && Files[i].clu == dp->obj.startCluster && Files[i].ofs == dp->currentReadWriteOffset)
				break;
		}

		if (i == _NB_FS_LOCK) { /* Not opened. Register it as new. */
			for (i = 0; i < _NB_FS_LOCK && Files[i].fs; i++)
				;
			if (i == _NB_FS_LOCK)
				return 0; /* No free entry to register (int err) */
			Files[i].fs = dp->obj.fs;
			Files[i].clu = dp->obj.startCluster;
			Files[i].ofs = dp->currentReadWriteOffset;
			Files[i].ctr = 0;
		}

		if (acc && Files[i].ctr)
			return 0; /* Access violation (int err) */

		Files[i].ctr = acc ? 0x100 : Files[i].ctr + 1; /* Set semaphore value */

		return i + 1;
	}

	static FRESULT dec_lock( /* Decrement object open counter */
	UINT i /* Semaphore index (1..) */
	) {
		WORD n;
		FRESULT res;

		if (--i < _NB_FS_LOCK) { /* Shift index number origin from 0 */
			n = Files[i].ctr;
			if (n == 0x100)
				n = 0; /* If write mode open, delete the entry */
			if (n > 0)
				n--; /* Decrement read mode open count */
			Files[i].ctr = n;
			if (n == 0)
				Files[i].fs = 0; /* Delete the entry if open count gets zero */
			res = FR_OK;
		} else {
			res = FR_INT_ERROR; /* Invalid index nunber */
		}
		return res;
	}

	static void clear_lock(/* Clear lock entries of the volume */
	FATFS *fs) {
		UINT i;

		for (i = 0; i < _NB_FS_LOCK; i++) {
			if (Files[i].fs == fs)
				Files[i].fs = 0;
		}
	}

#endif /* _NB_FS_LOCK != 0 */

	/*-----------------------------------------------------------------------*/
	/* Move/Flush disk access window in the file system object               */
	/*-----------------------------------------------------------------------*/
#if !_NB_FS_READONLY

	struct sync_window_strut {
		FATFS *fs;
		DWORD wsect;
		UINT nf;
	};

	static void sync_window_loop(sync_window_strut *strut) {
		if (strut->nf >= 2) { /* Reflect the change to all FAT copies */
			strut->wsect += strut->fs->fatSectorSize;
			voidPtr = strut;
			disk_write(strut->fs->driveNumber, strut->fs->win, strut->wsect, 1, [](DRESULT dRes) {
				sync_window_strut *strut = ((sync_window_strut*) voidPtr);
				strut->nf--;
				sync_window_loop(strut);
				return;
			});
			return;
		} else {
			delete strut;
			callNextCallback();
			return;
		}
	}

	static void sync_window(FATFS *fs, void (*callback)(void*), void *data) {
		sync_window_strut *s = new sync_window_strut();
		s->fs = fs;
		voidPtr = s;
		fResult = FR_OK;
		addCallback(callback, data);
		if (s->fs->wflag) { /* Write back the sector if it is dirty */
			voidPtr = s;
			disk_write(s->fs->driveNumber, s->fs->win, s->fs->winSector, 1, [](DRESULT dRes) {
				sync_window_strut *strut = ((sync_window_strut*) voidPtr);
				strut->wsect = strut->fs->winSector;
				if (dRes != RES_OK) {
					fResult = FR_DISK_ERROR;
				} else {
					strut->fs->wflag = 0;
					if (strut->wsect - strut->fs->fatBaseSector < strut->fs->fatSectorSize) { /* Is it in the FAT area? */
						strut->nf = strut->fs->n_fats;
						sync_window_loop(strut);
						return;
					}
				}
				delete strut;
				callNextCallback();
				return;
			});
			return;
		}
		delete s;
		callNextCallback();
		return;
	}
#endif

	struct move_window_strut {
		FATFS *fs;
		DWORD sector;
	};

	static void move_window(/* Returns FR_OK or FR_DISK_ERROR */
	FATFS *fs, DWORD sector, /* Sector number to make appearance in the fs->win[] */
	void (*callback)(void*), void *data) {
		move_window_strut *s = new move_window_strut();
		fResult = FR_OK;
		s->fs = fs;
		s->sector = sector;
		addCallback(callback, data);

		if (s->sector != s->fs->winSector) { /* Window offset changed? */
#if !_NB_FS_READONLY
			sync_window(s->fs, [](void *data) { /* Write-back changes */
				move_window_strut *strut = ((move_window_strut*) data);				
#endif
				if (fResult == FR_OK) { /* Fill sector window with new data */
					voidPtr = strut;
					disk_read(strut->fs->driveNumber, strut->fs->win, strut->sector, 1, [](DRESULT dRes) {
						move_window_strut *strut = ((move_window_strut*) voidPtr);
						if (dRes != RES_OK) {
							strut->sector = 0xFFFFFFFF; /* Invalidate window if data is not reliable */
							fResult = FR_DISK_ERROR;
						}
						strut->fs->winSector = strut->sector;
						delete strut;
						callNextCallback();
						return;
					});
					return;
				}
				delete strut;
				callNextCallback();
				return;
#if !_NB_FS_READONLY
			}, s);
			return;
#endif
		}
		delete s;
		callNextCallback();
		return;
	}

#if !_NB_FS_READONLY
	/*-----------------------------------------------------------------------*/
	/* Synchronize file system and strage device                             */
	/*-----------------------------------------------------------------------*/
	struct sync_fs_strut {
		FATFS *fs;
	};

	static void sync_fs(FATFS *fs, /* FR_OK:succeeded, !=0:error */
	void (*callback)(void*), void *data /* File system object */
	) {
		sync_fs_strut *s = new sync_fs_strut();
		s->fs = fs;
		addCallback(callback, data);
		sync_window(s->fs, [](void *data) {
			sync_fs_strut *strut = ((sync_fs_strut*) data);
			if (fResult == FR_OK) {
				/* Update FSInfo sector if needed */
				if (strut->fs->fs_type == FS_FAT32 && strut->fs->fsi_flag == 1) {
					/* Create FSInfo structure */
					mem_set(strut->fs->win, 0, SS(strut->fs));
					st_word(strut->fs->win + BS_55AA, 0xAA55);
					st_dword(strut->fs->win + FSI_LeadSig, 0x41615252);
					st_dword(strut->fs->win + FSI_StrucSig, 0x61417272);
					st_dword(strut->fs->win + FSI_Free_Count, strut->fs->numberOfFreeClusters);
					st_dword(strut->fs->win + FSI_Nxt_Free, strut->fs->lastCluster);
					/* Write it into the FSInfo sector */
					strut->fs->winSector = strut->fs->volBaseSector + 1;
					voidPtr = strut;
					disk_write(strut->fs->driveNumber, strut->fs->win, strut->fs->winSector, 1, [](DRESULT dRes) {
						sync_fs_strut *strut = ((sync_fs_strut*) voidPtr);
						strut->fs->fsi_flag = 0;
						/* Make sure that no pending write process in the physical drive */
						if (disk_ioctl(strut->fs->driveNumber, CTRL_SYNC, 0) != RES_OK) {
							fResult = FR_DISK_ERROR;
						}
						delete strut;
						callNextCallback();
						return;
					});
					return;
				}
				/* Make sure that no pending write process in the physical drive */
				if (disk_ioctl(strut->fs->driveNumber, CTRL_SYNC, 0) != RES_OK) {
					fResult = FR_DISK_ERROR;
				}
				delete strut;
				callNextCallback();
				return;
			} else {
				delete strut;
				callNextCallback();
				return;
			}
		}, s);
		return;
	}

#endif

	/*-----------------------------------------------------------------------*/
	/* Get sector# from cluster#                                             */
	/*-----------------------------------------------------------------------*/

	static DWORD clust2sect( /* !=0:Sector number, 0:Failed (invalid cluster#) */
	FATFS *fs, /* File system object */
	DWORD clst /* Cluster# to be converted */
	) {
		clst -= 2;
		if (clst >= fs->numberOfFatEntries - 2){
			return 0; /* Invalid cluster# */
		}
		return clst * fs->clusterSize + fs->dataBaseSector;
	}

	/*-----------------------------------------------------------------------*/
	/* FAT access - Read value of a FAT entry                                */
	/*-----------------------------------------------------------------------*/

	struct get_fat_strut {
		_FDID *obj;
		DWORD clst;
		UINT wc, bc;
		DWORD val;
		FATFS *fs;
	};

	static void get_fat( /* 0xFFFFFFFF:Disk error, 1:Internal error, 2..0x7FFFFFFF:Cluster status */
	_FDID *obj, /* Corresponding object */
	DWORD clst, /* Cluster number to get the value */
	void (*callback)(void*), void *data) {
		get_fat_strut *s = new get_fat_strut();
		s->obj = obj;
		s->clst = clst;
		s->fs = obj->fs;
		addCallback(callback, data);

		if (s->clst < 2 || s->clst >= s->fs->numberOfFatEntries) { /* Check if in valid range */
			getFatValue = 1; /* Internal error */
			delete s;
			callNextCallback();
			return;
		} else {
			getFatValue = 0xFFFFFFFF; /* Default value falls on disk error */

			switch (s->fs->fs_type) {
				case FS_FAT12:
					// bc = (UINT)clst; bc += bc / 2;
					// if (move_window(fs, fs->fatBaseSector + (bc / SS(fs))) != FR_OK) break;
					// wc = fs->win[bc++ % SS(fs)];
					// if (move_window(fs, fs->fatBaseSector + (bc / SS(fs))) != FR_OK) break;
					// wc |= fs->win[bc % SS(fs)] << 8;
					// val = (clst & 1) ? (wc >> 4) : (wc & 0xFFF);
					break;

				case FS_FAT16:
					move_window(s->fs, s->fs->fatBaseSector + (s->clst / (SS(s->fs) / 2)),
					[](void *data){
						get_fat_strut *strut = ((get_fat_strut*) data);
						if (fResult == FR_OK){
							getFatValue = ld_word(strut->fs->win + strut->clst * 2 % SS(strut->fs));
						}
						delete strut;
						callNextCallback();
						return;
					},s);
					return;

				case FS_FAT32:
					move_window(s->fs, s->fs->fatBaseSector + (s->clst / (SS(s->fs) / 4)), [](void *data) {
						get_fat_strut *strut = ((get_fat_strut*) data);
						if (fResult == FR_OK) {
							getFatValue = ld_dword(strut->fs->win + strut->clst * 4 % SS(strut->fs)) & 0x0FFFFFFF;
						}
						delete strut;
						callNextCallback();
						return;
					}, s);
					return;
#if _NB_FS_EXFAT
				case FS_EXFAT:
					if (s->obj->objSize) {
						DWORD cofs = s->clst - s->obj->startCluster; /* Offset from start cluster */
						DWORD clen = (DWORD) ((s->obj->objSize - 1) / SS(s->fs)) / s->fs->clusterSize; /* Number of clusters - 1 */

						if (s->obj->stat == 2) { /* Is there no valid chain on the FAT? */
							if (cofs <= clen) {
								getFatValue = (cofs == clen) ? 0x7FFFFFFF : s->clst + 1; /* Generate the value */
								break;
							}
						}
						if (s->obj->stat == 3 && cofs < s->obj->firstFragSize) { /* Is it in the 1st fragment? */
							getFatValue = s->clst + 1; /* Generate the value */
							break;
						}
						if (s->obj->stat != 2) { /* Get value from FAT if FAT chain is valid */
							if (s->obj->lastFragSize != 0) { /* Is it on the growing edge? */
								getFatValue = 0x7FFFFFFF; /* Generate EOC */
							} else {
								move_window(s->fs, s->fs->fatBaseSector + (s->clst / (SS(s->fs) / 4)), [](void *data) {
									get_fat_strut *strut = ((get_fat_strut*) data);
									if (fResult == FR_OK) {
										getFatValue = ld_dword(strut->fs->win + strut->clst * 4 % SS(strut->fs)) & 0x7FFFFFFF;
									}
									delete strut;
									callNextCallback();
									return;
								}, s);
								return;
							}
							break;
						}
					}
					/* go to default */
#endif
				default:
					getFatValue = 1; /* Internal error */
			}
			delete s;
			callNextCallback();
			return;
		}
	}

#if !_NB_FS_READONLY
	/*-----------------------------------------------------------------------*/
	/* FAT access - Change value of a FAT entry                              */
	/*-----------------------------------------------------------------------*/
	struct put_fat_strut {
		FATFS *fs;
		DWORD clst;
		DWORD val;
		UINT bc;
		BYTE *p;
	};

	static void put_fat(/* FR_OK(0):succeeded, !=0:error */
	FATFS *fs, DWORD clstInput, /* FAT index number (cluster number) to be changed */
	DWORD val, /* New value to be set to the entry */
	void (*callback)(void*), void *data) {
		put_fat_strut *s = new put_fat_strut();
		s->clst = clstInput;
		s->val = val;
		s->fs = fs;
		addCallback(callback, data);
		fResult = FR_INT_ERROR;

		if (s->clst >= 2 && s->clst < s->fs->numberOfFatEntries) { /* Check if in valid range */
			switch (s->fs->fs_type) {
				case FS_FAT12: /* Bitfield items */
					// bc = (UINT)clst; bc += bc / 2;
					// res = move_window(fs, fs->fatBaseSector + (bc / SS(fs)));
					// if (res != FR_OK) break;
					// p = fs->win + bc++ % SS(fs);
					// *p = (clst & 1) ? ((*p & 0x0F) | ((BYTE)val << 4)) : (BYTE)val;
					// fs->wflag = 1;
					// res = move_window(fs, fs->fatBaseSector + (bc / SS(fs)));
					// if (res != FR_OK) break;
					// p = fs->win + bc % SS(fs);
					// *p = (clst & 1) ? (BYTE)(val >> 4) : ((*p & 0xF0) | ((BYTE)(val >> 8) & 0x0F));
					// fs->wflag = 1;
					break;

				case FS_FAT16: /* WORD aligned items */
					// res = move_window(fs, fs->fatBaseSector + (clst / (SS(fs) / 2)));
					// if (res != FR_OK) break;
					// st_word(fs->win + clst * 2 % SS(fs), (WORD)val);
					// fs->wflag = 1;
					break;

				case FS_FAT32: /* DWORD aligned items */
#if _NB_FS_EXFAT
				case FS_EXFAT:
#endif
					move_window(s->fs, s->fs->fatBaseSector + (s->clst / (SS(s->fs) / 4)), [](void *data) {
						put_fat_strut *strut = ((put_fat_strut*) data);
						if (fResult == FR_OK) {
							if (!_NB_FS_EXFAT || strut->fs->fs_type != FS_EXFAT) {
								strut->val = (strut->val & 0x0FFFFFFF) | (ld_dword(strut->fs->win + strut->clst * 4 % SS(strut->fs)) & 0xF0000000);
							}
							st_dword(strut->fs->win + strut->clst * 4 % SS(strut->fs), strut->val);
							strut->fs->wflag = 1;
						}
						delete strut;
						callNextCallback();
						return;
					}, s);
					return;
			}
		}
		delete s;
		callNextCallback();
		return;
	}

#endif /* !_FS_READONLY */

#if _NB_FS_EXFAT && !_NB_FS_READONLY
	/*-----------------------------------------------------------------------*/
	/* exFAT: Accessing FAT and Allocation Bitmap                            */
	/*-----------------------------------------------------------------------*/

	/*--------------------------------------*/
	/* Find a contiguous free cluster block */
	/*--------------------------------------*/
	struct find_bitmap_strut {
		FATFS *fs;
		DWORD clst;
		DWORD ncl;
		BYTE bm;
		BYTE bv;
		UINT i;
		DWORD val;
		DWORD scl;
		DWORD ctr;
	};

	static void find_bitmap_loop(find_bitmap_strut *strut) {
		move_window(strut->fs, strut->fs->dataBaseSector + strut->val / 8 / SS(strut->fs), [](void *data) {
			find_bitmap_strut *strut = ((find_bitmap_strut*) data);
			if (fResult == FR_OK) {
				UINT i;
				i = strut->val / 8 % SS(strut->fs);
				strut->bm = 1 << (strut->val % 8);
				do {
					do {
						strut->bv = strut->fs->win[i] & strut->bm;
						strut->bm <<= 1; /* Get bit value */
						if (++strut->val >= strut->fs->numberOfFatEntries - 2) { /* Next cluster (with wrap-around) */
							strut->val = 0;
							strut->bm = 0;
							i = SS(strut->fs);
						}
						if (!strut->bv) { /* Is it a free cluster? */
							if (++strut->ctr == strut->ncl) { /* Check if run length is sufficient for required */
								findBitmapValue = strut->scl + 2;
								delete strut;
								callNextCallback();
								return;
							}
						} else {
							strut->scl = strut->val;
							strut->ctr = 0; /* Encountered a cluster in-use, restart to scan */
						}
						if (strut->val == strut->clst) { /* All cluster scanned? */
							findBitmapValue = 0;
							delete strut;
							callNextCallback();
							return;
						}
					} while (strut->bm);
					strut->bm = 1;
				} while (++i < SS(strut->fs));
				find_bitmap_loop(strut);
				return;
			} else {
				findBitmapValue = 0xFFFFFFFF;
				delete strut;
				callNextCallback();
				return;
			}
		}, strut);
		return;
	}

	static void find_bitmap( /* 0:Not found, 2..:Cluster block found, 0xFFFFFFFF:Disk error */
	FATFS *fs, /* File system object */
	DWORD clst, /* Cluster number to scan from */
	DWORD ncl, /* Number of contiguous clusters to find (1..) */
	void (*callback)(void*), void *data) {
		find_bitmap_strut *s = new find_bitmap_strut();
		s->ncl = ncl;
		s->clst = clst;
		s->fs = fs;
		addCallback(callback, data);

		s->clst -= 2; /* The first bit in the bitmap corresponds to cluster #2 */
		if (s->clst >= s->fs->numberOfFatEntries - 2) {
			s->clst = 0;
		}
		s->scl = s->val = s->clst;
		s->ctr = 0;
		find_bitmap_loop(s);
		return;
	}

	/*----------------------------------------*/
	/* Set/Clear a block of allocation bitmap */
	/*----------------------------------------*/
	struct change_bitmap_strut {
		FATFS *fs;
		DWORD clst;
		DWORD ncl;
		int bv;
		BYTE bm;
		UINT i;
		DWORD sect;
	};

	void change_bitmap_loop(change_bitmap_strut *strut) {
		move_window(strut->fs, strut->sect++, [](void *data) {
			change_bitmap_strut *strut = ((change_bitmap_strut*) data);
			if (fResult == FR_OK) {
				do {
					do {
						if (strut->bv == (int) ((strut->fs->win[strut->i] & strut->bm) != 0)) {
							fResult = FR_INT_ERROR;
							delete strut;
							callNextCallback();
							return;
						} /* Is the bit expected value? */
						strut->fs->win[strut->i] ^= strut->bm; /* Flip the bit */
						strut->fs->wflag = 1;
						if (--strut->ncl == 0) {
							fResult = FR_OK; /* All bits processed? */
							delete strut;
							callNextCallback();
							return;
						}
					} while (strut->bm <<= 1); /* Next bit */
					strut->bm = 1;
				} while (++strut->i < SS(strut->fs)); /* Next byte */
				strut->i = 0;
				change_bitmap_loop(strut);
				return;
			} else {
				fResult = FR_DISK_ERROR;
				delete strut;
				callNextCallback();
				return;
			}
		}, strut);
		return;
	}

	static void change_bitmap(FATFS *fs, /* File system object */
	DWORD clst, /* Cluster number to change from */
	DWORD ncl, /* Number of clusters to be changed */
	int bv, /* bit value to be set (0 or 1) */
	void (*callback)(void*), void *data) {
		change_bitmap_strut *s = new change_bitmap_strut();
		s->ncl = ncl;
		s->clst = clst;
		s->bv = bv;
		s->fs = fs;

		s->clst -= 2; /* The first bit corresponds to cluster #2 */
		s->sect = s->fs->dataBaseSector + s->clst / 8 / SS(s->fs); /* Sector address (assuming bitmap is located top of the cluster heap) */
		s->i = s->clst / 8 % SS(s->fs); /* Byte offset in the sector */
		s->bm = 1 << (s->clst % 8); /* Bit mask in the byte */

		addCallback(callback, data);
		change_bitmap_loop(s);
		return;
	}

	/*---------------------------------------------*/
	/* Fill the first fragment of the FAT chain    */
	/*---------------------------------------------*/
	struct fill_first_frag_strut {
		_FDID *obj;
		DWORD cl;
		DWORD n;
	};

	static void fill_first_frag_loop(fill_first_frag_strut *strut) {

		put_fat(strut->obj->fs, strut->cl, strut->cl + 1, [](void *data) {
			fill_first_frag_strut *strut = ((fill_first_frag_strut*) data);
			if (fResult != FR_OK) {
				delete strut;
				callNextCallback();
				return;
			}

			strut->n--;
			strut->cl++;
			if (strut->n) {
				fill_first_frag_loop(strut);
				return;
			} else {
				strut->obj->stat = 0; /* Change status 'FAT chain is valid' */
				delete strut;
				callNextCallback();
				return;
			}
		}, strut);
		return;
	}

	static void fill_first_frag(_FDID *obj, /* Pointer to the corresponding object */
	void (*callback)(void*), void *data) {
		addCallback(callback, data);
		fill_first_frag_strut *s = new fill_first_frag_strut();
		s->obj = obj;

		s->cl = s->obj->startCluster;
		s->n = s->obj->firstFragSize;

		if (s->obj->stat == 3) { /* Has the object been changed 'fragmented'? */
			fill_first_frag_loop(s);
			return;
		} else {
			fResult = FR_OK;
			delete s;
			callNextCallback();
			return;
		}
	}

	/*---------------------------------------------*/
	/* Fill the last fragment of the FAT chain     */
	/*---------------------------------------------*/
	struct fill_last_frag_strut {
		_FDID *obj;
		DWORD lcl;
		DWORD term;
	};

	void fill_last_frag_loop(fill_last_frag_strut *strut) {
		if (strut->obj->lastFragSize > 0) {
			/* Create the last chain on the FAT */
			put_fat(strut->obj->fs, strut->lcl - strut->obj->lastFragSize + 1,
					(strut->obj->lastFragSize > 1) ? strut->lcl - strut->obj->lastFragSize + 2 : strut->term, [](void *data) {
						fill_last_frag_strut *strut = ((fill_last_frag_strut*) data);
						if (fResult != FR_OK) {
							delete strut;
							callNextCallback();
							return;
						}
						strut->obj->lastFragSize--;
						fill_last_frag_loop(strut);
						return;
					}, strut);
			return;
		} else {
			fResult = FR_OK;
			delete strut;
			callNextCallback();
			return;
		}
	}

	static void fill_last_frag(_FDID *obj, /* Pointer to the corresponding object */
	DWORD lcl, /* Last cluster of the fragment */
	DWORD term, /* Value to set the last FAT entry */
	void (*callback)(void*), void *data) {
		addCallback(callback, data);
		fill_last_frag_strut *s = new fill_last_frag_strut();
		s->lcl = lcl;
		s->term = term;
		s->obj = obj;

		fill_last_frag_loop(s);
		return;
	}

#endif /* _NB_FS_EXFAT && !_FS_READONLY */

#if !_NB_FS_READONLY
	/*-----------------------------------------------------------------------*/
	/* FAT handling - Remove a cluster chain                                 */
	/*-----------------------------------------------------------------------*/
	struct remove_chain_strut {
		_FDID *obj;
		DWORD clst;
		DWORD pclst;
		DWORD nxt;
		FATFS *fs;
#if _NB_FS_EXFAT || _NB_USE_TRIM
		DWORD scl;
		DWORD ecl;
#endif
#if _NB_USE_TRIM
#error
		DWORD rt[2];
#endif
	};

	static void remove_chain_end(remove_chain_strut *strut) {
#if _NB_FS_EXFAT
		if (strut->fs->fs_type == FS_EXFAT) {
			if (strut->pclst == 0) { /* Does the object have no chain? */
				strut->obj->stat = 0; /* Change the object status 'initial' */
			} else {
				if (strut->obj->stat == 3 && strut->pclst >= strut->obj->startCluster
						&& strut->pclst <= strut->obj->startCluster + strut->obj->firstFragSize) { /* Did the chain get contiguous? */
					strut->obj->stat = 2; /* Change the object status 'contiguous' */
				}
			}
		}
#endif
		fResult = FR_OK;
		delete strut;
		callNextCallback();
		return;
	}
	static void remove_chain_loop_c(remove_chain_strut *strut) {
		strut->clst = strut->nxt; /* Next cluster */
		if (strut->clst < strut->fs->numberOfFatEntries) { /* Repeat while not the last link */
			remove_chain_loop_c(strut);
			return;
		} else {
			remove_chain_end(strut);
			return;
		}
	}

	static void remove_chain_loop_b(remove_chain_strut *strut) {
#if _NB_USE_TRIM
#error
		rt[0] = clust2sect(fs, scl);					   /* Start sector */
		rt[1] = clust2sect(fs, ecl) + fs->clusterSize - 1; /* End sector */
		disk_ioctl(fs->driveNumber, CTRL_TRIM, rt);		   /* Inform device the block can be erased */
#endif
		strut->scl = strut->ecl = strut->nxt;
		remove_chain_loop_c(strut);
		return;
	}

	static void remove_chain_loop_a(remove_chain_strut *strut) {
		if (strut->fs->numberOfFreeClusters < strut->fs->numberOfFatEntries - 2) { /* Update FSINFO */
			strut->fs->numberOfFreeClusters++;
			strut->fs->fsi_flag |= 1;
		}
#if _NB_FS_EXFAT || _NB_USE_TRIM
		if (strut->ecl + 1 == strut->nxt) { /* Is next cluster contiguous? */
			strut->ecl = strut->nxt;
		} else { /* End of contiguous cluster block */
#if _NB_FS_EXFAT
			if (strut->fs->fs_type == FS_EXFAT) {
				change_bitmap(strut->fs, strut->scl, strut->ecl - strut->scl + 1, 0, [](void *data) {
					remove_chain_strut *strut = ((remove_chain_strut*) data);
					if (fResult != FR_OK) {
						// return fResult;
						delete strut;
						callNextCallback();
						return;
					} else {
						remove_chain_loop_b(strut);
						return;
					}
				}, strut);
				return; /* Mark the cluster block 'free' on the bitmap */
			}
#endif
			remove_chain_loop_b(strut);
			return;
		}
#endif
		remove_chain_loop_c(strut);
		return;
	}

	static void remove_chain_loop(remove_chain_strut *strut) {
		get_fat(strut->obj, strut->clst, [](void *data) {
			remove_chain_strut *strut = ((remove_chain_strut*) data);
			strut->nxt = getFatValue;
			if (strut->nxt == 0) {
				remove_chain_end(strut);
				return;
			} /* Empty cluster? */
			if (strut->nxt == 1) {
				fResult = FR_INT_ERROR;
				delete strut;
				callNextCallback();
				return;
			} /* Internal error? */
			if (strut->nxt == 0xFFFFFFFF) {
				fResult = FR_DISK_ERROR;
				delete strut;
				callNextCallback();
				return;
			} /* Disk error? */
			if (!_NB_FS_EXFAT || strut->fs->fs_type != FS_EXFAT) {
				put_fat(strut->fs, strut->clst, 0, [](void *data) {
					remove_chain_strut *strut = ((remove_chain_strut*) data);
					if (fResult != FR_OK) {
						delete strut;
						callNextCallback();
						return;
					} else {
						remove_chain_loop_a(strut);
						return;
					}
				}, strut); /* Mark the cluster 'free' on the FAT */
				return;
			}
			remove_chain_loop_a(strut);
			return;
		}, strut);
		return; /* Get cluster status */
	}

	static void remove_chain( /* FR_OK(0):succeeded, !=0:error */
	_FDID *obj, /* Corresponding object */
	DWORD clst, /* Cluster to remove a chain from */
	DWORD pclst, /* Previous cluster of clst (0:an entire chain) */
	void (*callback)(void*), void *data) {
		addCallback(callback, data);
		remove_chain_strut *s = new remove_chain_strut();
		fResult = FR_OK;
		s->obj = obj;
		s->pclst = pclst;
		s->clst = clst;
		s->fs = s->obj->fs;
#if _NB_FS_EXFAT || _NB_USE_TRIM
		s->scl = s->clst;
		s->ecl = s->clst;
#endif
#if _NB_USE_TRIM
		DWORD rt[2];
#endif

		if (s->clst < 2 || s->clst >= s->fs->numberOfFatEntries) {
			fResult = FR_INT_ERROR;
			delete s;
			callNextCallback();
			return;
		} /* Check if in valid range */

		/* Mark the previous cluster 'EOC' on the FAT if it exists */
		if (s->pclst && (!_NB_FS_EXFAT || s->fs->fs_type != FS_EXFAT || s->obj->stat != 2)) {
			put_fat(s->fs, s->pclst, 0xFFFFFFFF, [](void *data) {
				remove_chain_strut *strut = ((remove_chain_strut*) data);
				if (fResult != FR_OK) {
					delete strut;
					callNextCallback();
					return;
				} else {
					remove_chain_loop(strut);
					return;
				}
			}, s);
			return;
		}
		/* Remove the chain */
		remove_chain_loop(s);
		return;
	}

	/*-----------------------------------------------------------------------*/
	/* FAT handling - Stretch a chain or Create a new chain                  */
	/*-----------------------------------------------------------------------*/
	struct create_chain_strut {
		_FDID *obj;
		DWORD clst;
		DWORD cs;
		DWORD ncl;
		DWORD scl;
		FATFS *fs;
	};

	static void create_chain_end(create_chain_strut *strut) {
		if (fResult == FR_OK) { /* Update FSINFO if function succeeded. */
			strut->fs->lastCluster = strut->ncl;
			if (strut->fs->numberOfFreeClusters <= strut->fs->numberOfFatEntries - 2) {
				strut->fs->numberOfFreeClusters--;
			}
			strut->fs->fsi_flag |= 1;
		} else {
			strut->ncl = (fResult == FR_DISK_ERROR) ? 0xFFFFFFFF : 1; /* Failed. Generate error status */
		}
		createChainValue = strut->ncl; /* Return new cluster number or error status */
		delete strut;
		callNextCallback();
		return;
	}

	static void create_chain_b(create_chain_strut *strut) {
		find_bitmap(strut->fs, strut->scl, 1, [](void *data) {
			create_chain_strut *strut = ((create_chain_strut*) data);
			strut->ncl = findBitmapValue;
			if (strut->ncl == 0 || strut->ncl == 0xFFFFFFFF) {
				createChainValue = strut->ncl;
				delete strut;
				callNextCallback();
				return;

			} /* No free cluster or hard error? */
			change_bitmap(strut->fs, strut->ncl, 1, 1, [](void *data) {
				create_chain_strut *strut = ((create_chain_strut*) data);
				if (fResult == FR_INT_ERROR) {
					createChainValue = 1;
					delete strut;
					callNextCallback();
					return;
				}
				if (fResult == FR_DISK_ERROR) {
					createChainValue = 0xFFFFFFFF;
					delete strut;
					callNextCallback();
					return;
				}
				if (strut->clst == 0) { /* Is it a new chain? */
					strut->obj->stat = 2; /* Set status 'contiguous' */
				} else { /* It is a stretched chain */
					if (strut->obj->stat == 2 && strut->ncl != strut->scl + 1) { /* Is the chain got fragmented? */
						strut->obj->firstFragSize = strut->scl - strut->obj->startCluster; /* Set size of the contiguous part */
						strut->obj->stat = 3; /* Change status 'just fragmented' */
					}
				}
				if (strut->obj->stat != 2) { /* Is the file non-contiguous? */
					if (strut->ncl == strut->clst + 1) { /* Is the cluster next to previous one? */
						strut->obj->lastFragSize = strut->obj->lastFragSize ? strut->obj->lastFragSize + 1 : 2; /* Increment size of last framgent */
					} else { /* New fragment */
						if (strut->obj->lastFragSize == 0) {
							strut->obj->lastFragSize = 1;
						}
						fill_last_frag(strut->obj, strut->clst, strut->ncl, [](void *data) {
							create_chain_strut *strut = ((create_chain_strut*) data);
							if (fResult == FR_OK) {
								strut->obj->lastFragSize = 1;
							}
							create_chain_end(strut);
							return;
						}, strut); /* Fill last fragment on the FAT and link it to new one */
						return;
					}
				}
				create_chain_end(strut);
				return;
			},strut);
			return; /* Mark the cluster 'in use' */
		}, strut);
		return; /* Find a free cluster */
	}

	static void create_chain_c_end(create_chain_strut *strut) {
		put_fat(strut->fs, strut->ncl, 0xFFFFFFFF, [](void *data) {
			create_chain_strut *strut = ((create_chain_strut*) data);
			if (fResult == FR_OK && strut->clst != 0) {
				put_fat(strut->fs, strut->clst, strut->ncl, [](void *data) {
					create_chain_strut *strut = ((create_chain_strut*) data);
					create_chain_end(strut);
					return;
				},strut); /* Link it from the previous one if needed */
				return;
			}
			create_chain_end(strut);
			return;
		}, strut);
		return; /* Mark the new cluster 'EOC' */
	}

	static void create_chain_c(create_chain_strut *strut) {
		strut->ncl++; /* Next cluster */
		if (strut->ncl >= strut->fs->numberOfFatEntries) { /* Check wrap-around */
			strut->ncl = 2;
			if (strut->ncl > strut->scl) {
				createChainValue = 0;
				delete strut;
				callNextCallback();
				return;
			} /* No free cluster */
		}
		get_fat(strut->obj, strut->ncl, [](void *data) {
			create_chain_strut *strut = ((create_chain_strut*) data);
			strut->cs = getFatValue;
			if (strut->cs == 0) {
				create_chain_c_end(strut);
				return;
			} /* Found a free cluster */
			if (strut->cs == 1 || strut->cs == 0xFFFFFFFF) {
				createChainValue = strut->cs;
				delete strut;
				callNextCallback();
				return;
			} /* An error occurred */
			if (strut->ncl == strut->scl) {
				createChainValue = 0;
				delete strut;
				callNextCallback();
				return;
			} /* No free cluster */
			create_chain_c(strut);
			return;
		}, strut);
		return; /* Get the cluster status */
	}

	static void create_chain_a(create_chain_strut *strut) {

#if _NB_FS_EXFAT
		if (strut->fs->fs_type == FS_EXFAT) { /* On the exFAT volume */
			create_chain_b(strut);
			return;
		} else
#endif
		{ /* On the FAT12/16/32 volume */
			strut->ncl = strut->scl; /* Start cluster */
			create_chain_c(strut);
			return;
		}
	}

	static void create_chain( /* 0:No free cluster, 1:Internal error, 0xFFFFFFFF:Disk error, >=2:New cluster# */
	_FDID *obj, /* Corresponding object */
	DWORD clst, /* Cluster# to stretch, 0:Create a new chain */
	void (*callback)(void*), void *data) {
		addCallback(callback, data);
		create_chain_strut *s = new create_chain_strut();
		s->obj = obj;
		s->clst = clst;
		s->fs = obj->fs;

		if (s->clst == 0) { /* Create a new chain */
			s->scl = s->fs->lastCluster; /* Get suggested cluster to start from */
			if (s->scl == 0 || s->scl >= s->fs->numberOfFatEntries) {
				s->scl = 1;
			}
			create_chain_a(s);
			return;
		} else { /* Stretch current chain */
			get_fat(s->obj, s->clst, [](void *data) {
				create_chain_strut *strut = ((create_chain_strut*) data);
				strut->cs = getFatValue;
				if (strut->cs < 2) {
					createChainValue = 1;
					delete strut;
					callNextCallback();
					return;
				} /* Invalid FAT value */
				if (strut->cs == 0xFFFFFFFF) {
					createChainValue = strut->cs;
					delete strut;
					callNextCallback();
					return;
				} /* A disk error occurred */
				if (strut->cs < strut->fs->numberOfFatEntries) {
					createChainValue = strut->cs;
					delete strut;
					callNextCallback();
					return;
				} /* It is already followed by next cluster */
				strut->scl = strut->clst;
				create_chain_a(strut);
				return;
			}, s);
			return; /* Check the cluster status */
		}
	}

#endif /* !_FS_READONLY */

#if _NB_USE_FASTSEEK
	/*-----------------------------------------------------------------------*/
	/* FAT handling - Convert offset into cluster with link map table        */
	/*-----------------------------------------------------------------------*/

	static DWORD clmt_clust( /* <2:Error, >=2:Cluster number */
	FIL *fp, /* Pointer to the file object */
	FSIZE_t ofs /* File offset to be converted to cluster# */
	) {
		DWORD cl, ncl, *tbl;
		FATFS *fs = fp->obj.fs;

		tbl = fp->clusterLinkMapTablePtr + 1; /* Top of CLMT */
		cl = (DWORD) (ofs / SS(fs) / fs->clusterSize); /* Cluster order from top of the file */
		for (;;) {
			ncl = *tbl++; /* Number of cluters in the fragment */
			if (ncl == 0)
				return 0; /* End of table? (error) */
			if (cl < ncl)
				break; /* In this fragment? */
			cl -= ncl;
			tbl++; /* Next fragment */
		}
		return cl + *tbl; /* Return the cluster number */
	}

#endif /* _NB_USE_FASTSEEK */

	/*-----------------------------------------------------------------------*/
	/* Directory handling - Set directory index                              */
	/*-----------------------------------------------------------------------*/
	struct dir_setDirectoryIndex_strut {
		DIR *dp;
		DWORD ofs;
		DWORD csz;
		DWORD clst;
		FATFS *fs;
	};

	static void dir_setDirectoryIndex_end(dir_setDirectoryIndex_strut *strut) {
		strut->dp->currentCluster = strut->clst; /* Current cluster# */
		if (!strut->dp->currentSector) {
			fResult = FR_INT_ERROR;
			delete strut;
			callNextCallback();
			return;
		}
		strut->dp->currentSector += strut->ofs / SS(strut->fs); /* Sector# of the directory entry */
		strut->dp->dirPtr = strut->fs->win + (strut->ofs % SS(strut->fs)); /* Pointer to the entry in the win[] */

		fResult = FR_OK;
		delete strut;
		callNextCallback();
		return;
	}

	static void dir_setDirectoryIndex_loop(dir_setDirectoryIndex_strut *strut) {
		if (strut->ofs >= strut->csz) { /* Follow cluster chain */
			get_fat(&strut->dp->obj, strut->clst, /* Get next cluster */
			[](void *data) {
				dir_setDirectoryIndex_strut *strut = ((dir_setDirectoryIndex_strut*) data);
				strut->clst = getFatValue;
				if (strut->clst == 0xFFFFFFFF) {
					fResult = FR_INT_ERROR;
					delete strut;
					callNextCallback();
					return;
				} /* Disk error */
				if (strut->clst < 2 || strut->clst >= strut->fs->numberOfFatEntries) {
					fResult = FR_INT_ERROR;
					delete strut;
					callNextCallback();
					return;
				} /* Reached to end of table or internal error */
				strut->ofs -= strut->csz;
				dir_setDirectoryIndex_loop(strut);
				return;
			}, strut);
			return;
		} else {
			strut->dp->currentSector = clust2sect(strut->fs, strut->clst);
			dir_setDirectoryIndex_end(strut);
			return;
		}
	}

	static void dir_setDirectoryIndex( /* FR_OK(0):succeeded, !=0:error */
	DIR *dp, /* Pointer to directory object */
	DWORD ofs, /* Offset of directory table */
	void (*callback)(void*), void *data) {
		dir_setDirectoryIndex_strut *s = new dir_setDirectoryIndex_strut();
		s->fs = dp->obj.fs;
		s->ofs = ofs;
		s->dp = dp;
		addCallback(callback, data);

		if (s->ofs >= (DWORD) ((_NB_FS_EXFAT && s->fs->fs_type == FS_EXFAT) ?
		MAX_DIR_EX : MAX_DIR) || s->ofs % SZDIRE) { /* Check range of offset and alignment */
			fResult = FR_INT_ERROR;
			delete s;
			callNextCallback();
			return;
		}
		s->dp->currentReadWriteOffset = s->ofs; /* Set current offset */
		s->clst = s->dp->obj.startCluster; /* Table start cluster (0:root) */
		if (s->clst == 0 && s->fs->fs_type >= FS_FAT32) { /* Replace cluster# 0 with root cluster# */
			s->clst = s->fs->dirBaseSector;
			if (_NB_FS_EXFAT) {
				s->dp->obj.stat = 0;
			} /* exFAT: Root dirPtr has an FAT chain */
		}

		if (s->clst == 0) { /* Static table (root-directory in FAT12/16) */
			if (s->ofs / SZDIRE >= s->fs->numberOfRootDirectoryEntries) {
				fResult = FR_INT_ERROR;
				delete s;
				callNextCallback();
				return;
			} /* Is index out of range? */
			s->dp->currentSector = s->fs->dirBaseSector;
		} else { /* Dynamic table (sub-directory or root-directory in FAT32+) */
			s->csz = (DWORD) s->fs->clusterSize * SS(s->fs); /* Bytes per cluster */
			dir_setDirectoryIndex_loop(s);
			return;
		}
		dir_setDirectoryIndex_end(s);
		return;
	}

	/*-----------------------------------------------------------------------*/
	/* Directory handling - Move directory table index next                  */
	/*-----------------------------------------------------------------------*/
	struct dir_next_strut {
		DIR *dp;
		int stretch;
		DWORD ofs;
		DWORD clst;
		FATFS *fs;
#if !_NB_FS_READONLY
		UINT n;
#endif
	};

	static void dir_next_end(dir_next_strut *strut) {
		strut->dp->currentReadWriteOffset = strut->ofs; /* Current entry */
		strut->dp->dirPtr = strut->fs->win + strut->ofs % SS(strut->fs); /* Pointer to the entry in the win[] */
		fResult = FR_OK;
		delete strut;
		callNextCallback();
		return;
	}

	static void dir_next_a_end(dir_next_strut *strut) {
		strut->dp->currentCluster = strut->clst; /* Initialize data for new cluster */
		strut->dp->currentSector = clust2sect(strut->fs, strut->clst);
		dir_next_end(strut);
		return;
	}

	static void dir_next_b(dir_next_strut *strut) {
		strut->fs->winSector -= strut->n; /* Restore window offset */
#if !_NB_FS_READONLY
#else
#error
		if (!strut->stretch)
		{
			strut->dp->currentSector = 0; /* (this line is to suppress compiler warning) */
		}
		strut->dp->currentSector = 0; /* Report EOT */
		fResult = FR_NO_FILE;
		delete strut;
		callNextCallback();
		return;
#endif
		dir_next_a_end(strut);
		return;
	}

	static void dir_next_c_loop(dir_next_strut *strut) { /* Clear window buffer */
		if (strut->n < strut->fs->clusterSize) { /* Fill the new cluster with 0 */
			strut->fs->wflag = 1;
			sync_window(strut->fs, [](void *data) {
				dir_next_strut *strut = ((dir_next_strut*) data);
				if (fResult != FR_OK) {
					fResult = FR_DISK_ERROR;
					delete strut;
					callNextCallback();
					return;
				}
				strut->n++;
				strut->fs->winSector++;
				dir_next_c_loop(strut);
				return;
			}, strut);
			return;
		}
		dir_next_b(strut);
		return;
	}

	static void dir_next_a(dir_next_strut *strut) {
		get_fat(&strut->dp->obj, strut->dp->currentCluster, [](void *data) {
			dir_next_strut *strut = ((dir_next_strut*) data);
			strut->clst = getFatValue;
			if (strut->clst <= 1) {
				fResult = FR_INT_ERROR;
				delete strut;
				callNextCallback();
				return;
			} /* Internal error */
			if (strut->clst == 0xFFFFFFFF) {
				fResult = FR_DISK_ERROR;
				delete strut;
				callNextCallback();
				return;
			} /* Disk error */
			if (strut->clst >= strut->fs->numberOfFatEntries) { /* Reached end of dynamic table */
#if !_NB_FS_READONLY
			if (!strut->stretch) { /* If no stretch, report EOT */
				strut->dp->currentSector = 0;
				fResult = FR_NO_FILE;
				delete strut;
				callNextCallback();
				return;
			}
			create_chain(&strut->dp->obj, strut->dp->currentCluster, [](void *data) {
				dir_next_strut *strut = ((dir_next_strut*) data);
				strut->clst = createChainValue;
				if (strut->clst == 0) {
					fResult = FR_DENIED;
					delete strut;
					callNextCallback();
					return;
				} /* No free cluster */
				if (strut->clst == 1) {
					fResult = FR_INT_ERROR;
					delete strut;
					callNextCallback();
					return;
				} /* Internal error */
				if (strut->clst == 0xFFFFFFFF) {
					fResult = FR_DISK_ERROR;
					delete strut;
					callNextCallback();
					return;
				} /* Disk error */
				/* Clean-up the stretched table */
				if (_NB_FS_EXFAT) {
					strut->dp->obj.stat |= 4;
				} /* The directory needs to be updated */
				sync_window(strut->fs, [](void *data) {
					dir_next_strut *strut = ((dir_next_strut*) data);
					if (fResult != FR_OK) {
						fResult = FR_DISK_ERROR;
						delete strut;
						callNextCallback();
						return;
					}
					mem_set(strut->fs->win, 0, SS(strut->fs));

					strut->n = 0;
					strut->fs->winSector = clust2sect(strut->fs, strut->clst);
					dir_next_c_loop(strut);
					return;
#endif
					dir_next_b(strut);
					return;
				},strut);
				return;
			}, strut); /* Allocate a cluster */
			return;
		}
		dir_next_a_end(strut);
		return;
	}, strut);
		return;
	}

	static void dir_next( /* FR_OK(0):succeeded, FR_NO_FILE:End of table, FR_DENIED:Could not stretch */
	DIR *dp, /* Pointer to the directory object */
	int stretch, /* 0: Do not stretch table, 1: Stretch table if needed */
	void (*callback)(void*), void *data) {
		dir_next_strut *s = new dir_next_strut();
		addCallback(callback, data);
		s->stretch = stretch;
		s->dp = dp;
		s->fs = dp->obj.fs;

		s->ofs = s->dp->currentReadWriteOffset + SZDIRE; /* Next entry */
		if (!s->dp->currentSector || s->ofs >= (DWORD) ((_NB_FS_EXFAT && s->fs->fs_type == FS_EXFAT) ?
		MAX_DIR_EX : MAX_DIR)) { /* Report EOT when offset has reached max value */
			fResult = FR_NO_FILE;
			delete s;
			callNextCallback();
			return;
		}

		if (s->ofs % SS(s->fs) == 0) { /* Sector changed? */
			s->dp->currentSector++; /* Next sector */
			if (!s->dp->currentCluster) { /* Static table */
				if (s->ofs / SZDIRE >= s->fs->numberOfRootDirectoryEntries) { /* Report EOT if it reached end of static table */
					s->dp->currentSector = 0;
					fResult = FR_NO_FILE;
					delete s;
					callNextCallback();
					return;
				}
			} else { /* Dynamic table */
				if ((s->ofs / SS(s->fs) & (s->fs->clusterSize - 1)) == 0) { /* Cluster changed? */
					dir_next_a(s);
					return;
				}
			}
		}
		dir_next_end(s);
		return;
	}

#if !_NB_FS_READONLY
	/*-----------------------------------------------------------------------*/
	/* Directory handling - Reserve a block of directory entries             */
	/*-----------------------------------------------------------------------*/
	struct dir_alloc_strut {
		DIR *dp;
		UINT nent;
		UINT n;
		FATFS *fs;
	};

	static void dir_alloc_end(dir_alloc_strut *strut) {
		if (fResult == FR_NO_FILE) {
			fResult = FR_DENIED;
		} /* No directory entry to allocate */
		delete strut;
		callNextCallback();
	}

	static void dir_alloc_loop(dir_alloc_strut *strut) {
		move_window(strut->fs, strut->dp->currentSector, [](void *data) {
			dir_alloc_strut *strut = ((dir_alloc_strut*) data);
			if (fResult != FR_OK) {
				dir_alloc_end(strut);
				return;
			}
#if _NB_FS_EXFAT
			if ((strut->fs->fs_type == FS_EXFAT) ? (int) ((strut->dp->dirPtr[XDIR_Type] & 0x80) == 0) : (int) (strut->dp->dirPtr[DIR_Name] == DDEM || strut->dp->dirPtr[DIR_Name] == 0))
		{
#else
			// if (strut->dp->dirPtr[DIR_Name] == DDEM || strut->dp->dirPtr[DIR_Name] == 0) {
#endif
			if (++strut->n == strut->nent)
			{ /* A block of contiguous free entries is found */
				dir_alloc_end(strut);
				return;
			}
		}
		else
		{
			strut->n = 0; /* Not a blank entry. Restart to search */
		}
			dir_next(strut->dp, 1, [](void *data) {
				dir_alloc_strut *strut = ((dir_alloc_strut*) data);
				if (fResult == FR_OK) { /* Next entry with table stretch enabled */
					dir_alloc_loop(strut);
					return;
				} else {
					dir_alloc_end(strut);
					return;
				}
			}, strut);
			return;
		}, strut);
		return;
	}

	static void dir_alloc( /* FR_OK(0):succeeded, !=0:error */
	DIR *dp, /* Pointer to the directory object */
	UINT nent, /* Number of contiguous entries to allocate */
	void (*callback)(void*), void *data) {
		dir_alloc_strut *s = new dir_alloc_strut();
		addCallback(callback, data);
		s->fs = dp->obj.fs;
		s->dp = dp;
		s->nent = nent;

		dir_setDirectoryIndex(s->dp, 0, [](void *data) {
			dir_alloc_strut *strut = ((dir_alloc_strut*) data);
			if (fResult == FR_OK) {
				strut->n = 0;
				dir_alloc_loop(strut);
				return;
			}
			dir_alloc_end(strut);
			return;
		}, s);
		return;
	}

#endif /* !_FS_READONLY */

	/*-----------------------------------------------------------------------*/
	/* FAT: Directory handling - Load/Store start cluster number             */
	/*-----------------------------------------------------------------------*/

	static DWORD ld_clust( /* Returns the top cluster value of the SFN entry */
	FATFS *fs, /* Pointer to the fs object */
	const BYTE *dirPtr /* Pointer to the key entry */
	) {
		DWORD cl;
		cl = ld_word(dirPtr + DIR_FstClusLO);
		if (fs->fs_type == FS_FAT32) {
			cl |= (DWORD) ld_word(dirPtr + DIR_FstClusHI) << 16;
		}
		return cl;
	}

#if !_NB_FS_READONLY
	static void st_clust(FATFS *fs, /* Pointer to the fs object */
	BYTE *dirPtr, /* Pointer to the key entry */
	DWORD cl /* Value to be set */
	) {
		st_word(dirPtr + DIR_FstClusLO, (WORD) cl);
		if (fs->fs_type == FS_FAT32) {
			st_word(dirPtr + DIR_FstClusHI, (WORD) (cl >> 16));
		}
	}
#endif

#if _NB_USE_LFN != 0
	/*------------------------------------------------------------------------*/
	/* FAT-LFN: LFN handling                                                  */
	/*------------------------------------------------------------------------*/
	static const BYTE LfnOfs[] = { 1, 3, 5, 7, 9, 14, 16, 18, 20, 22, 24, 28, 30 }; /* Offset of LFN characters in the directory entry */

	/*--------------------------------------------------------*/
	/* FAT-LFN: Compare a part of file name with an LFN entry */
	/*--------------------------------------------------------*/
	static int cmp_lfn( /* 1:matched, 0:not matched */
	const WCHAR *lfnBuffer, /* Pointer to the LFN working buffer to be compared */
	BYTE *dirPtr /* Pointer to the directory entry containing the part of LFN */
	) {
		UINT i, s;
		WCHAR wc, uc;

		if (ld_word(dirPtr + LDIR_FstClusLO) != 0)
			return 0; /* Check LDIR_FstClusLO */

		i = ((dirPtr[LDIR_Ord] & 0x3F) - 1) * 13; /* Offset in the LFN buffer */

		for (wc = 1, s = 0; s < 13; s++) { /* Process all characters in the entry */
			uc = ld_word(dirPtr + LfnOfs[s]); /* Pick an LFN character */
			if (wc) {
				if (i >= _NB_MAX_LFN || ff_wtoupper(uc) != ff_wtoupper(lfnBuffer[i++])) { /* Compare it */
					return 0; /* Not matched */
				}
				wc = uc;
			} else {
				if (uc != 0xFFFF)
					return 0; /* Check filler */
			}
		}

		if ((dirPtr[LDIR_Ord] & LLEF) && wc && lfnBuffer[i])
			return 0; /* Last segment matched but different length */

		return 1; /* The part of LFN matched */
	}

#if _NB_FS_MINIMIZE <= 1 || _NB_FS_RPATH >= 2 || _NB_USE_LABEL || _NB_FS_EXFAT
	/*-----------------------------------------------------*/
	/* FAT-LFN: Pick a part of file name from an LFN entry */
	/*-----------------------------------------------------*/
	static int pick_lfn( /* 1:succeeded, 0:buffer overflow or invalid LFN entry */
	WCHAR *lfnBuffer, /* Pointer to the LFN working buffer */
	BYTE *dirPtr /* Pointer to the LFN entry */
	) {
		UINT i, s;
		WCHAR wc, uc;

		if (ld_word(dirPtr + LDIR_FstClusLO) != 0)
			return 0; /* Check LDIR_FstClusLO is 0 */

		i = ((dirPtr[LDIR_Ord] & ~LLEF) - 1) * 13; /* Offset in the LFN buffer */

		for (wc = 1, s = 0; s < 13; s++) { /* Process all characters in the entry */
			uc = ld_word(dirPtr + LfnOfs[s]); /* Pick an LFN character */
			if (wc) {
				if (i >= _NB_MAX_LFN)
					return 0; /* Buffer overflow? */
				lfnBuffer[i++] = wc = uc; /* Store it */
			} else {
				if (uc != 0xFFFF)
					return 0; /* Check filler */
			}
		}

		if (dirPtr[LDIR_Ord] & LLEF) { /* Put terminator if it is the last LFN part */
			if (i >= _NB_MAX_LFN)
				return 0; /* Buffer overflow? */
			lfnBuffer[i] = 0;
		}

		return 1; /* The part of LFN is valid */
	}
#endif

#if !_NB_FS_READONLY
	/*-----------------------------------------*/
	/* FAT-LFN: Create an entry of LFN entries */
	/*-----------------------------------------*/
	static void put_lfn(const WCHAR *lfn, /* Pointer to the LFN */
	BYTE *dirPtr, /* Pointer to the LFN entry to be created */
	BYTE ord, /* LFN order (1-20) */
	BYTE sum /* Checksum of the corresponding SFN */
	) {
		UINT i, s;
		WCHAR wc;

		dirPtr[LDIR_Chksum] = sum; /* Set checksum */
		dirPtr[LDIR_Attr] = AM_LFN; /* Set attribute. LFN entry */
		dirPtr[LDIR_Type] = 0;
		st_word(dirPtr + LDIR_FstClusLO, 0);

		i = (ord - 1) * 13; /* Get offset in the LFN working buffer */
		s = wc = 0;
		do {
			if (wc != 0xFFFF)
				wc = lfn[i++]; /* Get an effective character */
			st_word(dirPtr + LfnOfs[s], wc); /* Put it */
			if (wc == 0)
				wc = 0xFFFF; /* Padding characters for left locations */
		} while (++s < 13);
		if (wc == 0xFFFF || !lfn[i])
			ord |= LLEF; /* Last LFN part is the start of LFN sequence */
		dirPtr[LDIR_Ord] = ord; /* Set the LFN order */
	}

#endif /* !_FS_READONLY */
#endif /* _USE_LFN != 0 */

#if _NB_USE_LFN != 0 && !_NB_FS_READONLY
	/*-----------------------------------------------------------------------*/
	/* FAT-LFN: Create a Numbered SFN                                        */
	/*-----------------------------------------------------------------------*/

	static void gen_numname(BYTE *dst, /* Pointer to the buffer to store numbered SFN */
	const BYTE *src, /* Pointer to SFN */
	const WCHAR *lfn, /* Pointer to LFN */
	UINT seq /* Sequence number */
	) {
		BYTE ns[8], c;
		UINT i, j;
		WCHAR wc;
		DWORD sr;

		mem_cpy(dst, src, 11);

		if (seq > 5) { /* In case of many collisions, generate a hash number instead of sequential number */
			sr = seq;
			while (*lfn) { /* Create a CRC */
				wc = *lfn++;
				for (i = 0; i < 16; i++) {
					sr = (sr << 1) + (wc & 1);
					wc >>= 1;
					if (sr & 0x10000)
						sr ^= 0x11021;
				}
			}
			seq = (UINT) sr;
		}

		/* itoa (hexdecimal) */
		i = 7;
		do {
			c = (BYTE) ((seq % 16) + '0');
			if (c > '9')
				c += 7;
			ns[i--] = c;
			seq /= 16;
		} while (seq);
		ns[i] = '~';

		/* Append the number */
		for (j = 0; j < i && dst[j] != ' '; j++) {
			if (IsDBCS1(dst[j])) {
				if (j == i - 1)
					break;
				j++;
			}
		}
		do {
			dst[j++] = (i < 8) ? ns[i++] : ' ';
		} while (j < 8);
	}
#endif /* _USE_LFN != 0 && !_FS_READONLY */

#if _NB_USE_LFN != 0
	/*-----------------------------------------------------------------------*/
	/* FAT-LFN: Calculate checksum of an SFN entry                           */
	/*-----------------------------------------------------------------------*/

	static BYTE sum_sfn(const BYTE *dirPtr /* Pointer to the SFN entry */
	) {
		BYTE sum = 0;
		UINT n = 11;

		do {
			sum = (sum >> 1) + (sum << 7) + *dirPtr++;
		} while (--n);
		return sum;
	}

#endif /* _USE_LFN != 0 */

#if _NB_FS_EXFAT
	/*-----------------------------------------------------------------------*/
	/* exFAT: Checksum                                                       */
	/*-----------------------------------------------------------------------*/

	static WORD xdir_sum( /* Get checksum of the directoly block */
	const BYTE *dirPtr /* Directory entry block to be calculated */
	) {
		UINT i, szblk;
		WORD sum;

		szblk = (dirPtr[XDIR_NumSec] + 1) * SZDIRE;
		for (i = sum = 0; i < szblk; i++) {
			if (i == XDIR_SetSum) { /* Skip sum field */
				i++;
			} else {
				sum = ((sum & 1) ? 0x8000 : 0) + (sum >> 1) + dirPtr[i];
			}
		}
		return sum;
	}

	static WORD xname_sum( /* Get check sum (to be used as hash) of the name */
	const WCHAR *name /* File name to be calculated */
	) {
		WCHAR chr;
		WORD sum = 0;

		while ((chr = *name++) != 0) {
			chr = ff_wtoupper(chr); /* File name needs to be ignored case */
			sum = ((sum & 1) ? 0x8000 : 0) + (sum >> 1) + (chr & 0xFF);
			sum = ((sum & 1) ? 0x8000 : 0) + (sum >> 1) + (chr >> 8);
		}
		return sum;
	}

#if !_NB_FS_READONLY && _NB_USE_MKFS
	static DWORD xsum32(BYTE dat, /* Data to be sumed */
	DWORD sum /* Previous value */
	) {
		sum = ((sum & 1) ? 0x80000000 : 0) + (sum >> 1) + dat;
		return sum;
	}
#endif

#if _NB_FS_MINIMIZE <= 1 || _NB_FS_RPATH >= 2
	/*------------------------------------------------------*/
	/* exFAT: Get object information from a directory block */
	/*------------------------------------------------------*/

	static void get_xdir_info(BYTE *dirb, /* Pointer to the direcotry entry block 85+C0+C1s */
	FILINFO *fno /* Buffer to store the extracted file information */
	) {
		UINT di, si;
		WCHAR w;
#if !_NB_LFN_UNICODE
		UINT nc;
#endif
		/* Get file name */
		di = 0;
#if _NB_LFN_UNICODE
#error
		for (si = SZDIRE * 2; di < dirb[XDIR_NumName]; si += 2, di++)
		{
			if ((si % SZDIRE) == 0)
				si += 2;			/* Skip entry type field */
			w = ld_word(dirb + si); /* Get a character */
			if (di >= _NB_MAX_LFN)
			{
				di = 0;
				break;
			} /* Buffer overflow --> inaccessible object name */
			fno->fname[di] = w; /* Store it */
		}
#else
		for (si = SZDIRE * 2, nc = 0; nc < dirb[XDIR_NumName]; si += 2, nc++) {
			if ((si % SZDIRE) == 0)
				si += 2; /* Skip entry type field */
			w = ff_convert(ld_word(dirb + si), 0); /* Get a character and Unicode -> OEM */
			if (_DF1S && w >= 0x100) { /* Is it a double byte char? (always false at SBCS cfg) */
				fno->fname[di++] = (char) (w >> 8); /* Put 1st byte of the DBC */
			}
			if (w == 0 || di >= _NB_MAX_LFN) {
				di = 0;
				break;
			} /* Invalid char or buffer overflow --> inaccessible object name */
			fno->fname[di++] = (char) w;
		}
#endif
		if (di == 0)
			fno->fname[di++] = '?'; /* Inaccessible object name? */
		fno->fname[di] = 0; /* Terminate file name */

		fno->altname[0] = 0; /* No SFN */
		fno->fattrib = dirb[XDIR_Attr]; /* Attribute */
		fno->fsize = (fno->fattrib & AM_DIRECTORY) ? 0 : ld_qword(dirb + XDIR_FileSize); /* Size */
		fno->ftime = ld_word(dirb + XDIR_ModTime + 0); /* Time */
		fno->fdate = ld_word(dirb + XDIR_ModTime + 2); /* Date */
	}

#endif /* _NB_FS_MINIMIZE <= 1 || _NB_FS_RPATH >= 2 */

	/*-----------------------------------*/
	/* exFAT: Get a directry entry block */
	/*-----------------------------------*/
	struct load_xdir_strut {
		DIR *dp;
		UINT i;
		UINT sz_ent;
		BYTE *dirb;
	}
	;

	static void load_xdir_end(load_xdir_strut *strut) {
		/* Sanity check (do it when accessible object name) */
		if (strut->i <= MAXDIRB(_NB_MAX_LFN)) {
			if (xdir_sum(strut->dirb) != ld_word(strut->dirb + XDIR_SetSum)) {
				fResult = FR_INT_ERROR;
				delete strut;
				callNextCallback();
				return;
			}
		}
		fResult = FR_OK;
		delete strut;
		callNextCallback();
		return;
	}

	static void load_xdir_loop(load_xdir_strut *strut) {
		dir_next(strut->dp, 0, [](void *data) {
			load_xdir_strut *strut = ((load_xdir_strut*) data);
			if (fResult != FR_OK) {
				delete strut;
				callNextCallback();
				return;
			}
			move_window(strut->dp->obj.fs, strut->dp->currentSector, [](void *data) {
				load_xdir_strut *strut = ((load_xdir_strut*) data);
				if (fResult != FR_OK) {
					delete strut;
					callNextCallback();
					return;
				}
				if (strut->dp->dirPtr[XDIR_Type] != 0xC1) {
					fResult = FR_INT_ERROR;
					delete strut;
					callNextCallback();
					return;
				}
				if (strut->i < MAXDIRB(_NB_MAX_LFN)) {
					mem_cpy(strut->dirb + strut->i, strut->dp->dirPtr, SZDIRE);
				}

				if ((strut->i += SZDIRE) < strut->sz_ent) {
					load_xdir_loop(strut);
					return;
				} else {
					load_xdir_end(strut);
					return;
				}
			}, strut);
			return;
		}, strut);
		return;
	}

	static void load_xdir( /* FR_INT_ERROR: invalid entry block */
	DIR *dp, /* Pointer to the reading direcotry object pointing the 85 entry */
	void (*callback)(void*), void *data) {
		addCallback(callback, data);
		load_xdir_strut *s = new load_xdir_strut();
		s->dp = dp;
		s->dirb = s->dp->obj.fs->dirBuffer; /* Pointer to the on-memory direcotry entry block 85+C0+C1s */

		/* Load 85 entry */
		move_window(s->dp->obj.fs, s->dp->currentSector, [](void *data) {
			load_xdir_strut *strut = ((load_xdir_strut*) data);
			if (fResult != FR_OK) {
				delete strut;
				callNextCallback();
				return;
			}
			if (strut->dp->dirPtr[XDIR_Type] != 0x85) {
				fResult = FR_INT_ERROR;
				delete strut;
				callNextCallback();
				return;
			}
			mem_cpy(strut->dirb + 0, strut->dp->dirPtr, SZDIRE);
			strut->sz_ent = (strut->dirb[XDIR_NumSec] + 1) * SZDIRE;
			if (strut->sz_ent < 3 * SZDIRE || strut->sz_ent > 19 * SZDIRE) {
				fResult = FR_INT_ERROR;
				delete strut;
				callNextCallback();
				return;
			}

			/* Load C0 entry */
			dir_next(strut->dp, 0, [](void *data) {
				load_xdir_strut *strut = ((load_xdir_strut*) data);
				if (fResult != FR_OK) {
					delete strut;
					callNextCallback();
					return;
				}
				move_window(strut->dp->obj.fs, strut->dp->currentSector, [](void *data) {
					load_xdir_strut *strut = ((load_xdir_strut*) data);
					if (fResult != FR_OK) {
						delete strut;
						callNextCallback();
						return;
					}
					if (strut->dp->dirPtr[XDIR_Type] != 0xC0) {
						fResult = FR_INT_ERROR;
						delete strut;
						callNextCallback();
						return;
					}
					mem_cpy(strut->dirb + SZDIRE, strut->dp->dirPtr, SZDIRE);
					if (MAXDIRB(strut->dirb[XDIR_NumName]) > strut->sz_ent) {
						fResult = FR_INT_ERROR;
						delete strut;
						callNextCallback();
						return;
					}

					/* Load C1 entries */
					strut->i = SZDIRE * 2; /* C1 offset */
					load_xdir_loop(strut);
					return;
				},strut);
				return;
			},strut);
			return;
		}, s);
	}

#if !_NB_FS_READONLY || _NB_FS_RPATH != 0
	/*------------------------------------------------*/
	/* exFAT: Load the object's directory entry block */
	/*------------------------------------------------*/
	struct load_obj_strut {
		DIR *dp;
		const _FDID *obj;
	}
	;

	static void load_obj_dir(DIR *dp, /* Blank directory object to be used to access containing direcotry */
	const _FDID *obj, /* Object with its containing directory information */
	void (*callback)(void*), void *data) {
		addCallback(callback, data);
		load_obj_strut *s = new load_obj_strut();
		s->dp = dp;

		/* Open object containing directory */
		s->dp->obj.fs = obj->fs;
		s->dp->obj.startCluster = obj->containingDirectoryStartCluster;
		s->dp->obj.stat = (BYTE) obj->sizeOfContainingDirectory;
		s->dp->obj.objSize = obj->sizeOfContainingDirectory & 0xFFFFFF00;
		s->dp->currentEntryBlockOffset = obj->offsetInContainingDirectory;

		dir_setDirectoryIndex(s->dp, s->dp->currentEntryBlockOffset, [](void *data) {
			load_obj_strut *strut = ((load_obj_strut*) data);
			if (fResult == FR_OK) {
				load_xdir(strut->dp, [](void *data) {
					load_obj_strut *strut = ((load_obj_strut*) data);
					delete strut;
					callNextCallback();
					return;
				}, strut); /* Load the object's entry block */
				return;
			}
			delete strut;
			callNextCallback();
			return;
		}, s); /* Goto object's entry block */
	}
#endif

#if !_NB_FS_READONLY
	/*-----------------------------------------------*/
	/* exFAT: Store the directory block to the media */
	/*-----------------------------------------------*/
	struct store_xdir_strut {
		DIR *dp;
		UINT nent;
		BYTE *dirb;
	}
	;

	void static store_xdir_end(store_xdir_strut *strut) {
		(fResult == FR_OK || fResult == FR_DISK_ERROR) ? fResult = FR_OK : fResult = FR_INT_ERROR;
		delete strut;
		callNextCallback();
	}

	void static store_xdir_loop(store_xdir_strut *strut) {
		if (fResult == FR_OK) {
			move_window(strut->dp->obj.fs, strut->dp->currentSector, [](void *data) {
				store_xdir_strut *strut = ((store_xdir_strut*) data);
				if (fResult != FR_OK) {
					store_xdir_end(strut);
					return;
				}
				mem_cpy(strut->dp->dirPtr, strut->dirb, SZDIRE);
				strut->dp->obj.fs->wflag = 1;
				if (--strut->nent == 0) {
					store_xdir_end(strut);
					return;
				}
				strut->dirb += SZDIRE;
				dir_next(strut->dp, 0, [](void *data) {
					store_xdir_strut *strut = ((store_xdir_strut*) data);
					store_xdir_loop(strut);
					return;
				},strut);
				return;
			}, strut);
			return;
		} else {
			store_xdir_end(strut);
			return;
		}
	}

	static void store_xdir(DIR *dpInput, /* Pointer to the directory object */
	void (*callback)(void*), void *data) {
		addCallback(callback, data);
		store_xdir_strut *s = new store_xdir_strut();
		s->dp = dpInput;
		s->dirb = s->dp->obj.fs->dirBuffer; /* Pointer to the direcotry entry block 85+C0+C1s */

		/* Create set sum */
		st_word(s->dirb + XDIR_SetSum, xdir_sum(s->dirb));
		s->nent = s->dirb[XDIR_NumSec] + 1;

		/* Store the set of directory to the volume */
		dir_setDirectoryIndex(s->dp, s->dp->currentEntryBlockOffset, [](void *data) {
			store_xdir_strut *strut = ((store_xdir_strut*) data);
			store_xdir_loop(strut);
		}, s);
	}

	/*-------------------------------------------*/
	/* exFAT: Create a new directory enrty block */
	/*-------------------------------------------*/
	static void create_xdir(BYTE *dirb, /* Pointer to the direcotry entry block buffer */
	const WCHAR *lfn /* Pointer to the nul terminated file name */
	) {
		UINT i;
		BYTE nb, nc;
		WCHAR chr;

		/* Create 85+C0 entry */
		mem_set(dirb, 0, 2 * SZDIRE);
		dirb[XDIR_Type] = 0x85;
		dirb[XDIR_Type + SZDIRE] = 0xC0;

		/* Create C1 entries */
		nc = 0;
		nb = 1;
		chr = 1;
		i = SZDIRE * 2;
		do {
			dirb[i++] = 0xC1;
			dirb[i++] = 0; /* Entry type C1 */
			do { /* Fill name field */
				if (chr && (chr = lfn[nc]) != 0)
					nc++; /* Get a character if exist */
				st_word(dirb + i, chr); /* Store it */
			} while ((i += 2) % SZDIRE != 0);
			nb++;
		} while (lfn[nc]); /* Fill next entry if any char follows */

		dirb[XDIR_NumName] = nc; /* Set name length */
		dirb[XDIR_NumSec] = nb; /* Set block length */
		st_word(dirb + XDIR_NameHash, xname_sum(lfn)); /* Set name hash */
	}

#endif /* !_FS_READONLY */
#endif /* _NB_FS_EXFAT */

#if _NB_FS_MINIMIZE <= 1 || _NB_FS_RPATH >= 2 || _NB_USE_LABEL || _NB_FS_EXFAT
	/*-----------------------------------------------------------------------*/
	/* Read an object from the directory                                     */
	/*-----------------------------------------------------------------------*/
	struct dir_read_strut {
		DIR *dp;
		int vol;
		FATFS *fs;
		BYTE a;
		BYTE c;
#if _NB_USE_LFN
		BYTE ord = 0xFF;
		BYTE sum = 0xFF;
#endif
	}
	;

	static void dir_read_end(dir_read_strut *strut) {
		if (fResult != FR_OK)
			strut->dp->currentSector = 0; /* Terminate the read operation on error or EOT */
		delete strut;
		callNextCallback();
	}

	static void dir_read_loop(dir_read_strut *strut) {
		if (strut->dp->currentSector) {
			move_window(strut->dp->obj.fs, strut->dp->currentSector, [](void *data) {
				dir_read_strut *strut = ((dir_read_strut*) data);
				if (fResult != FR_OK) {
					dir_read_end(strut);
					return;
				}
				strut->c = strut->dp->dirPtr[DIR_Name]; /* Test for the entry type */
				if (strut->c == 0) {
					fResult = FR_NO_FILE;
					{
						dir_read_end(strut);
						return;
					} /* Reached to end of the directory */
				}
#if _NB_FS_EXFAT
				if (strut->fs->fs_type == FS_EXFAT) { /* On the exFAT volume */
					if (_NB_USE_LABEL && strut->vol) {
						if (strut->c == 0x83) {
							dir_read_end(strut);
							return;
						} /* Volume label entry? */
					} else {
						if (strut->c == 0x85) { /* Start of the file entry block? */
							strut->dp->currentEntryBlockOffset = strut->dp->currentReadWriteOffset; /* Get location of the block */
							load_xdir(strut->dp, [](void *data) {
								dir_read_strut *strut = ((dir_read_strut*) data);
								if (fResult == FR_OK) {
									strut->dp->obj.attr = strut->fs->dirBuffer[XDIR_Attr] & AM_MASK; /* Get attribute */
								}
								dir_read_end(strut);
								return;
							},strut); /* Load the entry block */
							return;
						}
					}
				} else
#endif
				{ /* On the FAT12/16/32 volume */
					strut->dp->obj.attr = strut->a = strut->dp->dirPtr[DIR_Attr] & AM_MASK; /* Get attribute */
#if _NB_USE_LFN != 0 /* LFN configuration */
				if (strut->c == DDEM || strut->c == '.' || (int) ((strut->a & ~AM_ARCHIVE) == AM_VOL) != strut->vol) { /* An entry without valid data */
					strut->ord = 0xFF;
				} else {
					if (strut->a == AM_LFN) { /* An LFN entry is found */
						if (strut->c & LLEF) { /* Is it start of an LFN sequence? */
							strut->sum = strut->dp->dirPtr[LDIR_Chksum];
							strut->c &= (BYTE) ~LLEF;
							strut->ord = strut->c;
							strut->dp->currentEntryBlockOffset = strut->dp->currentReadWriteOffset;
						}
						/* Check LFN validity and capture it */
						strut->ord = (strut->c == strut->ord && strut->sum == strut->dp->dirPtr[LDIR_Chksum] && pick_lfn(strut->fs->lfnBuffer, strut->dp->dirPtr)) ?
			strut->ord - 1 : 0xFF;
		} else { /* An SFN entry is found */
			if (strut->ord
			|| strut->sum
			!= sum_sfn(strut->dp->dirPtr)) { /* Is there a valid LFN? */
				strut->dp->currentEntryBlockOffset =
				0xFFFFFFFF; /* It has no LFN. */
			}
			dir_read_end(strut);
			return;
		}
	}
#else				 /* Non LFN configuration */
#error
	if (c != DDEM && c != '.' && a != AM_LFN && (int)((a & ~AM_ARCHIVE) == AM_VOL) == vol) { /* Is it a valid entry? */
		break;
	}
#endif
}
				dir_next(strut->dp, 0, [](void *data) {
					dir_read_strut *strut = ((dir_read_strut*) data);
					if (fResult != FR_OK) {
						dir_read_end(strut);
						return;
					}
					dir_read_loop(strut);
					return;
				}, strut); /* Next entry */
				return;
			}, strut);
			return;
		}
		else
		{
			dir_read_end(strut);
			return;
		}
	}

	static void dir_read(DIR *dp, /* Pointer to the directory object */
	int vol, /* Filtered by 0:file/directory or 1:volume label */
	void (*callback)(void*), void *data) {
		addCallback(callback, data);
		fResult = FR_NO_FILE;
		dir_read_strut *s = new dir_read_strut();
		s->dp = dp;
		s->fs = dp->obj.fs;
		s->vol = vol;
#if _NB_USE_LFN != 0
		s->ord = 0xFF, s->sum = 0xFF;
#endif

		dir_read_loop(s);
			return;
	}

#endif /* _NB_FS_MINIMIZE <= 1 || _NB_USE_LABEL || _NB_FS_RPATH >= 2 */

	/*-----------------------------------------------------------------------*/
	/* Directory handling - Find an object in the directory                  */
	/*-----------------------------------------------------------------------*/
	struct dir_find_strut {
		DIR *dp;
		FATFS *fs;
		BYTE c;
#if _NB_USE_LFN
		BYTE a;
		BYTE ord;
		BYTE sum;
#endif
#if _NB_FS_EXFAT
		WORD hash;
#endif
	}
	;

	static void dir_find_loop_a(dir_find_strut *strut) {
		dir_read(strut->dp, 0, [](void *data) {
			dir_find_strut *strut = ((dir_find_strut*) data);
			BYTE nc;
			UINT di, ni;

			if (fResult == FR_OK) {
#if _NB_MAX_LFN < 255
#error
				if (strut->fs->dirBuffer[XDIR_NumName] > _NB_MAX_LFN)
				{
					continue;
				} /* Skip comparison if inaccessible object name */
#endif
				if (ld_word(strut->fs->dirBuffer + XDIR_NameHash) != strut->hash) {
					dir_find_loop_a(strut);
					return;
				} /* Skip comparison if hash mismatched */
				for (nc = strut->fs->dirBuffer[XDIR_NumName], di = SZDIRE * 2, ni = 0; nc; nc--, di += 2, ni++) { /* Compare the name */
					if ((di % SZDIRE) == 0) {
						di += 2;
					}
					if (ff_wtoupper(ld_word(strut->fs->dirBuffer + di)) != ff_wtoupper(strut->fs->lfnBuffer[ni])) {
						break;
					}
				}
				if (nc == 0 && !strut->fs->lfnBuffer[ni]) {
					delete strut;
					callNextCallback();
					return;
				} /* Name matched? */
				dir_find_loop_a(strut);
				return;
			} else {
				delete strut;
				callNextCallback();
				return;
			}
		}, strut);
	}

	static void dir_find_loop_b(dir_find_strut *strut) {
		move_window(strut->fs, strut->dp->currentSector, [](void *data) {
			dir_find_strut *strut = ((dir_find_strut*) data);
			if (fResult != FR_OK) {
				delete strut;
				callNextCallback();
				return;
			};
			strut->c = strut->dp->dirPtr[DIR_Name];
			if (strut->c == 0) {
				fResult = FR_NO_FILE;
				delete strut;
				callNextCallback();
				return;
			} /* Reached to end of table */

#if _NB_USE_LFN != 0 /* LFN configuration */
			strut->dp->obj.attr = strut->a = strut->dp->dirPtr[DIR_Attr] & AM_MASK;
			if (strut->c == DDEM || ((strut->a & AM_VOL) && strut->a != AM_LFN)) { /* An entry without valid data */
				strut->ord = 0xFF;
				strut->dp->currentEntryBlockOffset = 0xFFFFFFFF; /* Reset LFN sequence */
			} else {
				if (strut->a == AM_LFN) { /* An LFN entry is found */
					if (!(strut->dp->fn[NSFLAG] & NS_NOLFN)) {
						if (strut->c & LLEF) { /* Is it start of LFN sequence? */
							strut->sum = strut->dp->dirPtr[LDIR_Chksum];
							strut->c &= (BYTE) ~LLEF;
							strut->ord = strut->c; /* LFN start order */
							strut->dp->currentEntryBlockOffset = strut->dp->currentReadWriteOffset; /* Start offset of LFN */
						}
						/* Check validity of the LFN entry and compare it with given name */
						strut->ord = (strut->c == strut->ord && strut->sum == strut->dp->dirPtr[LDIR_Chksum] && cmp_lfn(strut->fs->lfnBuffer, strut->dp->dirPtr)) ? strut->ord - 1 : 0xFF;
	}
}
else
{ /* An SFN entry is found */
	if (!strut->ord && strut->sum == sum_sfn(strut->dp->dirPtr))
	{
		delete strut;
		callNextCallback();
		return;
	} /* LFN matched? */
	if (!(strut->dp->fn[NSFLAG] & NS_LOSS) && !mem_cmp(strut->dp->dirPtr, strut->dp->fn, 11))
	{
		delete strut;
		callNextCallback();
		return;
	} /* SFN matched? */
	strut->ord = 0xFF;
	strut->dp->currentEntryBlockOffset = 0xFFFFFFFF; /* Reset LFN sequence */
}
}
#else				 /* Non LFN configuration */
#error
strut->dp->obj.attr = strut->dp->dirPtr[DIR_Attr] & AM_MASK;
if (!(strut->dp->dirPtr[DIR_Attr] & AM_VOL) && !mem_cmp(strut->dp->dirPtr, strut->dp->fn, 11))
{
delete strut;
callNextCallback();
return;
} /* Is it a valid entry? */
#endif
			dir_next(strut->dp, 0, [](void *data) {
				dir_find_strut *strut = ((dir_find_strut*) data);
				if (fResult == FR_OK) {
					dir_find_loop_b(strut);
					return;
				} else {
					delete strut;
					callNextCallback();
					return;
				}
			},strut);
			return;
		}, strut);
		return;
	}

	static void dir_find( /* FR_OK(0):succeeded, !=0:error */
	DIR *dpInput, /* Pointer to the directory object with the file name */
	void (*callback)(void*), void *data) {
		addCallback(callback, data);
		dir_find_strut *s = new dir_find_strut();
		s->dp = dpInput;
		s->fs = s->dp->obj.fs;

		dir_setDirectoryIndex(s->dp, 0, [](void *data) {
			dir_find_strut *strut = ((dir_find_strut*) data);
			if (fResult != FR_OK) {
				delete strut;
				callNextCallback();
				return;
			}
#if _NB_FS_EXFAT
			if (strut->fs->fs_type == FS_EXFAT) { /* On the exFAT volume */
				strut->hash = xname_sum(strut->fs->lfnBuffer); /* Hash value of the name to find */
				dir_find_loop_a(strut);
				return;
			}
#endif
			/* On the FAT12/16/32 volume */
#if _NB_USE_LFN != 0
			strut->ord = strut->sum = 0xFF;
			strut->dp->currentEntryBlockOffset = 0xFFFFFFFF; /* Reset LFN sequence */
#endif
			dir_find_loop_b(strut);
			return;
		}, s);
		return;
	}

#if !_NB_FS_READONLY
	/*-----------------------------------------------------------------------*/
	/* Register an object to the directory                                   */
	/*-----------------------------------------------------------------------*/
	struct dir_register_strut {
		DIR *dp;
		FATFS *fs;
#if _NB_USE_LFN
		UINT n;
		UINT nlen;
		UINT nent;
		BYTE sn[12];
		BYTE sum;
#endif
#if _NB_FS_EXFAT
		DIR dj;
#endif
	}
	;

	static void dir_register_a(dir_register_strut *strut) {
		create_xdir(strut->fs->dirBuffer, strut->fs->lfnBuffer); /* Create on-memory directory block to be written later */
		fResult = FR_OK;
		delete strut;
		callNextCallback();
		return;
	}

	static void dir_register_c_end(dir_register_strut *strut) {
		/* Set SFN entry */
		if (fResult == FR_OK) {
			move_window(strut->fs, strut->dp->currentSector, [](void *data) {
				dir_register_strut *strut = ((dir_register_strut*) data);
				if (fResult == FR_OK) {
					mem_set(strut->dp->dirPtr, 0, SZDIRE); /* Clean the entry */
					mem_cpy(strut->dp->dirPtr + DIR_Name, strut->dp->fn, 11); /* Put SFN */
#if _NB_USE_LFN != 0
				strut->dp->dirPtr[DIR_NTres] = strut->dp->fn[NSFLAG] & (NS_BODY | NS_EXT); /* Put NT flag */
#endif
				strut->fs->wflag = 1;
			}
			delete strut;
			callNextCallback();
			return;
		}, strut);
			return;
		}
		delete strut;
		callNextCallback();
		return;
	}

	static void dir_register_c_loop(dir_register_strut *strut) {
		/* Store LFN entries in bottom first */
		move_window(strut->fs, strut->dp->currentSector, [](void *data) {
			dir_register_strut *strut = ((dir_register_strut*) data);
			if (fResult != FR_OK) {
				dir_register_c_end(strut);
				return;
			}
			put_lfn(strut->fs->lfnBuffer, strut->dp->dirPtr, (BYTE) strut->nent, strut->sum);
			strut->fs->wflag = 1;
			dir_next(strut->dp, 0, [](void *data) {
				dir_register_strut *strut = ((dir_register_strut*) data);
				if (fResult == FR_OK && --strut->nent) {
					dir_register_c_loop(strut);
					return;
				} else {
					dir_register_c_end(strut);
					return;
				}
			},strut); /* Next entry */
			return;
		}, strut);
		return;
	}

	static void dir_register_c(dir_register_strut *strut) {
#if _NB_USE_LFN != 0  /* LFN configuration */
		/* Create an SFN with/without LFNs-> */
		strut->nent = (strut->sn[NSFLAG] & NS_LFN) ? (strut->nlen + 12) / 13 + 1 : 1; /* Number of entries to allocate */
		dir_alloc(strut->dp, strut->nent, [](void *data) {
			dir_register_strut *strut = static_cast<dir_register_strut*>(data);
			if (fResult == FR_OK && --strut->nent) { /* Set LFN entry if needed */
				dir_setDirectoryIndex(strut->dp, strut->dp->currentReadWriteOffset - strut->nent * SZDIRE, [](void *data) {
					dir_register_strut *strut = (dir_register_strut*)data;
					if (fResult == FR_OK) {
//						((dir_register_strut*)data)->sum = sum_sfn(((dir_register_strut*)data)->dp->fn); /* Checksum value of the SFN tied to the LFN */
//						dir_register_c_loop((dir_register_strut*)data);
						strut->sum = sum_sfn(strut->dp->fn); /* Checksum value of the SFN tied to the LFN */
						dir_register_c_loop(strut);
						return;
					}
//					dir_register_c_end((dir_register_strut*)data);
					dir_register_c_end(strut);
					return;
				}, strut);
				return;
			}
#else						   /* Non LFN configuration */
fResult = dir_alloc(strut->dp, 1); /* Allocate an entry for SFN */
#endif
			dir_register_c_end(strut);
			return;
		}, strut); /* Allocate entries */
		return;
	}

	static void dir_register_b(dir_register_strut *strut) {
		if (strut->n == 100) {
			fResult = FR_DENIED;
			delete strut;
			callNextCallback();
			return;
		} /* Abort if too many collisions */
		if (fResult != FR_NO_FILE) {
			delete strut;
			callNextCallback();
			return;
		} /* Abort if the result is other than 'not collided' */
		strut->dp->fn[NSFLAG] = strut->sn[NSFLAG];
		dir_register_c(strut);
		return;
	}

	static void dir_register_loop(dir_register_strut *strut) {
		if (strut->n < 100) {
			gen_numname(strut->dp->fn, strut->sn, strut->fs->lfnBuffer, strut->n); /* Generate a numbered name */
			dir_find(strut->dp, [](void *data) {
				dir_register_strut *strut = ((dir_register_strut*) data);
				if (fResult != FR_OK) {
					dir_register_b(strut);
					return;
				}
				strut->n++;
				dir_register_loop(strut);
				return;
			}, strut); /* Check if the name collides with existing SFN */
			return;
		} else {
			dir_register_b(strut);
			return;
		}
	}

	static void dir_register( /* FR_OK:succeeded, FR_DENIED:no free entry or too many SFN collision, FR_DISK_ERROR:disk error */
	DIR *dp, /* Target directory with object name to be created */
	void (*callback)(void*), void *data) {
		addCallback(callback, data);
		dir_register_strut *s = new dir_register_strut();
		s->dp = dp;
		s->fs = s->dp->obj.fs;
#if _NB_USE_LFN != 0 /* LFN configuration */

		if (s->dp->fn[NSFLAG] & (NS_DOT | NS_NONAME)) {
			fResult = FR_INVALID_NAME;
			delete s;
			callNextCallback();
			return;
		} /* Check name validity */
		for (s->nlen = 0; s->fs->lfnBuffer[s->nlen]; s->nlen++)
			; /* Get lfn length */

#if _NB_FS_EXFAT
		if (s->fs->fs_type == FS_EXFAT) { /* On the exFAT volume */

			s->nent = (s->nlen + 14) / 15 + 2; /* Number of entries to allocate (85+C0+C1s) */
			dir_alloc(s->dp, s->nent, [](void *data) {
				dir_register_strut *strut = ((dir_register_strut*) data);
				if (fResult != FR_OK) {
					delete strut;
					callNextCallback();
					return;
				}
				strut->dp->currentEntryBlockOffset = strut->dp->currentReadWriteOffset - SZDIRE * (strut->nent - 1); /* Set the allocated entry block offset */

				if (strut->dp->obj.startCluster != 0 && (strut->dp->obj.stat & 4)) { /* Has the sub-directory been stretched? */
					strut->dp->obj.objSize += (DWORD) strut->fs->clusterSize * SS(strut->fs); /* Increase the directory size by cluster size */
					fill_first_frag(&strut->dp->obj, [](void *data) {
						dir_register_strut *strut = ((dir_register_strut*) data);
						if (fResult != FR_OK) {
							delete strut;
							callNextCallback();
							return;
						}
						fill_last_frag(&strut->dp->obj, strut->dp->currentCluster, 0xFFFFFFFF, [](void *data) {
							dir_register_strut *strut = ((dir_register_strut*) data);
							if (fResult != FR_OK) {
								delete strut;
								callNextCallback();
								return;
							}
							load_obj_dir(&strut->dj, &strut->dp->obj, [](void *data) {
								dir_register_strut *strut = ((dir_register_strut*) data);
								if (fResult != FR_OK) {
									delete strut;
									callNextCallback();
									return;
								}
								st_qword(strut->fs->dirBuffer + XDIR_FileSize, strut->dp->obj.objSize); /* Update the allocation status */
								st_qword(strut->fs->dirBuffer + XDIR_ValidFileSize, strut->dp->obj.objSize);
								strut->fs->dirBuffer[XDIR_GenFlags] = strut->dp->obj.stat | 1;
								store_xdir(&strut->dj, [](void *data) { /* Store the object status */
									dir_register_strut *strut = ((dir_register_strut*) data);
									if (fResult != FR_OK) {
										dir_register_strut *strut = ((dir_register_strut*) data);
										delete strut;
										callNextCallback();
										return;
									}
									dir_register_a(strut);
									return;
								}, strut);
								return;
							}, strut); /* Load the object status */
							return;
						}, strut); /* Fill last fragment on the FAT if needed */
						return;
					}, strut); /* Fill first fragment on the FAT if needed */
					return;
				}
				dir_register_a(strut);
				return;
			}, s); /* Allocate entries */
			return;
		}
#endif
		/* On the FAT12/16/32 volume */
		mem_cpy(s->sn, s->dp->fn, 12);
		if (s->sn[NSFLAG] & NS_LOSS) { /* When LFN is out of 8.3 format, generate a numbered name */
			s->dp->fn[NSFLAG] = NS_NOLFN; /* Find only SFN */
			s->n = 1;
			dir_register_loop(s);
			return;
		}
#endif
		dir_register_c(s);
		return;
	}

#endif /* !_FS_READONLY */

#if !_NB_FS_READONLY && _NB_FS_MINIMIZE == 0
	/*-----------------------------------------------------------------------*/
	/* Remove an object from the directory                                   */
	/*-----------------------------------------------------------------------*/
	struct dir_remove_strut {
		DIR *dp;
		FATFS *fs;
#if _NB_USE_LFN
		DWORD last;
#endif
	}
	;

	static void dir_remove_loop_end(dir_remove_strut *strut) {
		if (fResult == FR_NO_FILE) {
			fResult = FR_INT_ERROR;
		}
		delete strut;
		callNextCallback();
	}

	static void dir_remove_loop(dir_remove_strut *strut) {
		move_window(strut->fs, strut->dp->currentSector, [](void *data) {
			dir_remove_strut *strut = ((dir_remove_strut*) data);
			if (fResult != FR_OK) {
				dir_remove_loop_end(strut);
				return;
			}
			/* Mark an entry 'deleted' */
			if (_NB_FS_EXFAT && strut->fs->fs_type == FS_EXFAT) { /* On the exFAT volume */
				strut->dp->dirPtr[XDIR_Type] &= 0x7F;
			} else { /* On the FAT12/16/32 volume */
				strut->dp->dirPtr[DIR_Name] = DDEM;
			}
			strut->fs->wflag = 1;
			if (strut->dp->currentReadWriteOffset >= strut->last) {
				dir_remove_loop_end(strut);
				return;
			} /* If reached last entry then all entries of the object has been deleted. */
			dir_next(strut->dp, 0, [](void *data) {
				dir_remove_strut *strut = ((dir_remove_strut*) data);
				if (fResult == FR_OK) {
					dir_remove_loop(strut);
					return;
				} else {
					if (fResult == FR_NO_FILE) {
						fResult = FR_INT_ERROR;
					}
					dir_remove_loop_end(strut);
					return;
				}
			}, strut); /* Next entry */
			return;
		}, strut);
		return;
	}

	static void dir_remove_a(dir_remove_strut *strut) {
		if (fResult == FR_OK) {
			dir_remove_loop(strut);
			return;
		} else {
			delete strut;
			callNextCallback();
			return;
		}
	}

	static void dir_remove( /* FR_OK:Succeeded, FR_DISK_ERROR:A disk error */
	DIR *dp, /* Directory object pointing the entry to be removed */
	void (*callback)(void*), void *data) {
		addCallback(callback, data);
		dir_remove_strut *s = new dir_remove_strut();
		s->dp = dp;
		s->fs = s->dp->obj.fs;
#if _NB_USE_LFN != 0 /* LFN configuration */
		s->last = s->dp->currentReadWriteOffset;

		if (s->dp->currentEntryBlockOffset == 0xFFFFFFFF) { /* Goto top of the entry block if LFN is exist */
			fResult = FR_OK;
			dir_remove_a(s);
			return;
		} else {
			dir_setDirectoryIndex(s->dp, s->dp->currentEntryBlockOffset, [](void *data) {
				dir_remove_strut *strut = ((dir_remove_strut*) data);
				dir_remove_a(strut);
				return;
			}, s);
			return;
		}

#else /* Non LFN configuration */
#error

	fResult = move_window(fs, strut->dp->currentSector);
	if (fResult == FR_OK)
	{
		strut->dp->dirPtr[DIR_Name] = DDEM;
		strut->fs->wflag = 1;
	}
#endif
	}

#endif /* !_FS_READONLY && _NB_FS_MINIMIZE == 0 */

#if _NB_FS_MINIMIZE <= 1 || _NB_FS_RPATH >= 2
	/*-----------------------------------------------------------------------*/
	/* Get file information from directory entry                             */
	/*-----------------------------------------------------------------------*/

	static void get_fileinfo( /* No return code */
	DIR *dp, /* Pointer to the directory object */
	FILINFO *fno /* Pointer to the file information to be filled */
	) {
		UINT i, j;
		TCHAR c;
		DWORD tm;
#if _NB_USE_LFN != 0
		WCHAR w, lfv;
		FATFS *fs = dp->obj.fs;
#endif

		fno->fname[0] = 0; /* Invaidate file info */
		if (!dp->currentSector)
			return; /* Exit if read pointer has reached end of directory */

#if _NB_USE_LFN != 0 /* LFN configuration */
#if _NB_FS_EXFAT
		if (fs->fs_type == FS_EXFAT) { /* On the exFAT volume */
			get_xdir_info(fs->dirBuffer, fno);
			return;
		} else
#endif
		{ /* On the FAT12/16/32 volume */
			if (dp->currentEntryBlockOffset != 0xFFFFFFFF) { /* Get LFN if available */
				i = j = 0;
				while ((w = fs->lfnBuffer[j++]) != 0) { /* Get an LFN character */
#if !_NB_LFN_UNICODE
					w = ff_convert(w, 0); /* Unicode -> OEM */
					if (w == 0) {
						i = 0;
						break;
					} /* No LFN if it could not be converted */
					if (_DF1S && w >= 0x100) { /* Put 1st byte if it is a DBC (always false at SBCS cfg) */
						fno->fname[i++] = (char) (w >> 8);
					}
#endif
					if (i >= _NB_MAX_LFN) {
						i = 0;
						break;
					} /* No LFN if buffer overflow */
					fno->fname[i++] = (TCHAR) w;
				}
				fno->fname[i] = 0; /* Terminate the LFN */
			}
		}

		i = j = 0;
		lfv = fno->fname[i]; /* LFN is exist if non-zero */
		while (i < 11) { /* Copy name body and extension */
			c = (TCHAR) dp->dirPtr[i++];
			if (c == ' ')
				continue; /* Skip padding spaces */
			if (c == RDDEM)
				c = (TCHAR) DDEM; /* Restore replaced DDEM character */
			if (i == 9) { /* Insert a . if extension is exist */
				if (!lfv)
					fno->fname[j] = '.';
				fno->altname[j++] = '.';
			}
#if _NB_LFN_UNICODE
#error
			if (IsDBCS1(c) && i != 8 && i != 11 && IsDBCS2(dp->dirPtr[i]))
			{
				c = c << 8 | dp->dirPtr[i++];
			}
			c = ff_convert(c, 1); /* OEM -> Unicode */
			if (!c)
				c = '?';
#endif
			fno->altname[j] = c;
			if (!lfv) {
				if (IsUpper(c) && (dp->dirPtr[DIR_NTres] & ((i >= 9) ? NS_EXT : NS_BODY))) {
					c += 0x20; /* To lower */
				}
				fno->fname[j] = c;
			}
			j++;
		}
		if (!lfv) {
			fno->fname[j] = 0;
			if (!dp->dirPtr[DIR_NTres])
				j = 0; /* Altname is no longer needed if neither LFN nor case info is exist. */
		}
		fno->altname[j] = 0; /* Terminate the SFN */

#else /* Non-LFN configuration */
#error
	i = j = 0;
	while (i < 11)
	{ /* Copy name body and extension */
		c = (TCHAR)dp->dirPtr[i++];
		if (c == ' ')
			continue; /* Skip padding spaces */
		if (c == RDDEM)
			c = (TCHAR)DDEM; /* Restore replaced DDEM character */
		if (i == 9)
			fno->fname[j++] = '.'; /* Insert a . if extension is exist */
		fno->fname[j++] = c;
	}
	fno->fname[j] = 0;
#endif

		fno->fattrib = dp->dirPtr[DIR_Attr]; /* Attribute */
		fno->fsize = ld_dword(dp->dirPtr + DIR_FileSize); /* Size */
		tm = ld_dword(dp->dirPtr + DIR_ModTime); /* Timestamp */
		fno->ftime = (WORD) tm;
		fno->fdate = (WORD) (tm >> 16);
	}

#endif /* _NB_FS_MINIMIZE <= 1 || _NB_FS_RPATH >= 2 */

#if _NB_USE_FIND && _NB_FS_MINIMIZE <= 1
#error
	/*-----------------------------------------------------------------------*/
	/* Pattern matching                                                      */
	/*-----------------------------------------------------------------------*/

	static WCHAR get_achar(					 /* Get a character and advances ptr 1 or 2 */
						   const TCHAR **ptr /* Pointer to pointer to the SBCS/DBCS/Unicode string */
	)
	{
#if !_NB_LFN_UNICODE
		WCHAR chr;

		chr = (BYTE) * (*ptr)++; /* Get a byte */
		if (IsLower(chr))
			chr -= 0x20; /* To upper ASCII char */
#ifdef _EXCVT
		if (chr >= 0x80)
			chr = ExCvt[chr - 0x80]; /* To upper SBCS extended char */
#else
		if (IsDBCS1(chr) && IsDBCS2(**ptr))
		{ /* Get DBC 2nd byte if needed */
			chr = chr << 8 | (BYTE) * (*ptr)++;
		}
#endif
		return chr;
#else
	return ff_wtoupper(*(*ptr)++); /* Get a word and to upper */
#endif
	}

	static int pattern_matching(				  /* 0:not matched, 1:matched */
								const TCHAR *pat, /* Matching pattern */
								const TCHAR *nam, /* String to be tested */
								int skip,		  /* Number of pre-skip chars (number of ?s) */
								int inf			  /* Infinite search (* specified) */
	)
	{
		const TCHAR *pp, *np;
		WCHAR pc, nc;
		int nm, nx;

		while (skip--)
		{ /* Pre-skip name chars */
			if (!get_achar(&nam))
				return 0; /* Branch mismatched if less name chars */
		}
		if (!*pat && inf)
			return 1; /* (short circuit) */

		do
		{
			pp = pat;
			np = nam; /* Top of pattern and name to match */
			for (;;)
			{
				if (*pp == '?' || *pp == '*')
				{ /* Wildcard? */
					nm = nx = 0;
					do
					{ /* Analyze the wildcard chars */
						if (*pp++ == '?')
							nm++;
						else
							nx = 1;
					} while (*pp == '?' || *pp == '*');
					if (pattern_matching(pp, np, nm, nx))
						return 1; /* Test new branch (recurs upto number of wildcard blocks in the pattern) */
					nc = *np;
					break; /* Branch mismatched */
				}
				pc = get_achar(&pp); /* Get a pattern char */
				nc = get_achar(&np); /* Get a name char */
				if (pc != nc)
					break; /* Branch mismatched? */
				if (pc == 0)
					return 1; /* Branch matched? (matched at end of both strings) */
			}
			get_achar(&nam); /* nam++ */
		} while (inf && nc); /* Retry until end of name if infinite search is specified */

		return 0;
	}

#endif /* _USE_FIND && _NB_FS_MINIMIZE <= 1 */

	/*-----------------------------------------------------------------------*/
	/* Pick a top segment and create the object name in directory form       */
	/*-----------------------------------------------------------------------*/

	static FRESULT create_name( /* FR_OK: successful, FR_INVALID_NAME: could not create */
	DIR *dp, /* Pointer to the directory object */
	const TCHAR **path /* Pointer to pointer to the segment in the path string */
	) {
#if _NB_USE_LFN != 0 /* LFN configuration */
		BYTE b, cf;
		WCHAR w, *lfn;
		UINT i, ni, si, di;
		const TCHAR *p;

		/* Create LFN in Unicode */
		p = *path;
		lfn = dp->obj.fs->lfnBuffer;
		si = di = 0;
		for (;;) {
			w = p[si++]; /* Get a character */
			if (w < ' ')
				break; /* Break if end of the path name */
			if (w == '/' || w == '\\') { /* Break if a separator is found */
				while (p[si] == '/' || p[si] == '\\')
					si++; /* Skip duplicated separator if exist */
				break;
			}
			if (di >= _NB_MAX_LFN)
				return FR_INVALID_NAME; /* Reject too long name */
#if !_NB_LFN_UNICODE
			w &= 0xFF;
			if (IsDBCS1(w)) { /* Check if it is a DBC 1st byte (always false on SBCS cfg) */
				b = (BYTE) p[si++]; /* Get 2nd byte */
				w = (w << 8) + b; /* Create a DBC */
				if (!IsDBCS2(b))
					return FR_INVALID_NAME; /* Reject invalid sequence */
			}
			w = ff_convert(w, 1); /* Convert ANSI/OEM to Unicode */
			if (!w)
				return FR_INVALID_NAME; /* Reject invalid code */
#endif
			if (w < 0x80 && chk_chr("\"*:<>\?|\x7F", w))
				return FR_INVALID_NAME; /* Reject illegal characters for LFN */
			lfn[di++] = w; /* Store the Unicode character */
		}
		*path = &p[si]; /* Return pointer to the next segment */
		cf = (w < ' ') ? NS_LAST : 0; /* Set last segment flag if end of the path */
#if _NB_FS_RPATH != 0
#error
		if ((di == 1 && lfn[di - 1] == '.') ||
			(di == 2 && lfn[di - 1] == '.' && lfn[di - 2] == '.'))
		{ /* Is this segment a dot name? */
			lfn[di] = 0;
			for (i = 0; i < 11; i++) /* Create dot name for SFN entry */
				dp->fn[i] = (i < di) ? '.' : ' ';
			dp->fn[i] = cf | NS_DOT; /* This is a dot entry */
			return FR_OK;
		}
#endif
		while (di) { /* Snip off trailing spaces and dots if exist */
			w = lfn[di - 1];
			if (w != ' ' && w != '.')
				break;
			di--;
		}
		lfn[di] = 0; /* LFN is created */
		if (di == 0)
			return FR_INVALID_NAME; /* Reject nul name */

		/* Create SFN in directory form */
		mem_set(dp->fn, ' ', 11);
		for (si = 0; lfn[si] == ' ' || lfn[si] == '.'; si++)
			; /* Strip leading spaces and dots */
		if (si)
			cf |= NS_LOSS | NS_LFN;
		while (di && lfn[di - 1] != '.')
			di--; /* Find extension (di<=si: no extension) */

		i = b = 0;
		ni = 8;
		for (;;) {
			w = lfn[si++]; /* Get an LFN character */
			if (!w)
				break; /* Break on end of the LFN */
			if (w == ' ' || (w == '.' && si != di)) { /* Remove spaces and dots */
				cf |= NS_LOSS | NS_LFN;
				continue;
			}

			if (i >= ni || si == di) { /* Extension or end of SFN */
				if (ni == 11) { /* Long extension */
					cf |= NS_LOSS | NS_LFN;
					break;
				}
				if (si != di)
					cf |= NS_LOSS | NS_LFN; /* Out of 8.3 format */
				if (si > di)
					break; /* No extension */
				si = di;
				i = 8;
				ni = 11; /* Enter extension section */
				b <<= 2;
				continue;
			}

			if (w >= 0x80) { /* Non ASCII character */
#ifdef _EXCVT
				w = ff_convert(w, 0); /* Unicode -> OEM code */
				if (w)
					w = ExCvt[w - 0x80]; /* Convert extended character to upper (SBCS) */
#else
			w = ff_convert(ff_wtoupper(w), 0); /* Upper converted Unicode -> OEM code */
#endif
				cf |= NS_LFN; /* Force create LFN entry */
			}

			if (_DF1S && w >= 0x100) { /* Is this DBC? (always false at SBCS cfg) */
				if (i >= ni - 1) {
					cf |= NS_LOSS | NS_LFN;
					i = ni;
					continue;
				}
				dp->fn[i++] = (BYTE) (w >> 8);
			} else { /* SBC */
				if (!w || chk_chr("+,;=[]", w)) { /* Replace illegal characters for SFN */
					w = '_';
					cf |= NS_LOSS | NS_LFN; /* Lossy conversion */
				} else {
					if (IsUpper(w)) { /* ASCII large capital */
						b |= 2;
					} else {
						if (IsLower(w)) { /* ASCII small capital */
							b |= 1;
							w -= 0x20;
						}
					}
				}
			}
			dp->fn[i++] = (BYTE) w;
		}

		if (dp->fn[0] == DDEM)
			dp->fn[0] = RDDEM; /* If the first character collides with DDEM, replace it with RDDEM */

		if (ni == 8)
			b <<= 2;
		if ((b & 0x0C) == 0x0C || (b & 0x03) == 0x03)
			cf |= NS_LFN; /* Create LFN entry when there are composite capitals */
		if (!(cf & NS_LFN)) { /* When LFN is in 8.3 format without extended character, NT flags are created */
			if ((b & 0x03) == 0x01)
				cf |= NS_EXT; /* NT flag (Extension has only small capital) */
			if ((b & 0x0C) == 0x04)
				cf |= NS_BODY; /* NT flag (Filename has only small capital) */
		}

		dp->fn[NSFLAG] = cf; /* SFN is created */

		return FR_OK;

#else /* _USE_LFN != 0 : Non-LFN configuration */
#error
	BYTE c, d, *sfn;
	UINT ni, si, i;
	const char *p;

	/* Create file name in directory form */
	p = *path;
	sfn = dp->fn;
	mem_set(sfn, ' ', 11);
	si = i = 0;
	ni = 8;
#if _NB_FS_RPATH != 0
	if (p[si] == '.')
	{ /* Is this a dot entry? */
		for (;;)
		{
			c = (BYTE)p[si++];
			if (c != '.' || si >= 3)
				break;
			sfn[i++] = c;
		}
		if (c != '/' && c != '\\' && c > ' ')
			return FR_INVALID_NAME;
		*path = p + si;										  /* Return pointer to the next segment */
		sfn[NSFLAG] = (c <= ' ') ? NS_LAST | NS_DOT : NS_DOT; /* Set last segment flag if end of the path */
		return FR_OK;
	}
#endif
	for (;;)
	{
		c = (BYTE)p[si++];
		if (c <= ' ')
			break; /* Break if end of the path name */
		if (c == '/' || c == '\\')
		{ /* Break if a separator is found */
			while (p[si] == '/' || p[si] == '\\')
				si++; /* Skip duplicated separator if exist */
			break;
		}
		if (c == '.' || i >= ni)
		{ /* End of body or over size? */
			if (ni == 11 || c != '.')
				return FR_INVALID_NAME; /* Over size or invalid dot */
			i = 8;
			ni = 11; /* Goto extension */
			continue;
		}
		if (c >= 0x80)
		{ /* Extended character? */
#ifdef _EXCVT
			c = ExCvt[c - 0x80]; /* To upper extended characters (SBCS cfg) */
#else
#if !_DF1S
			return FR_INVALID_NAME; /* Reject extended characters (ASCII only cfg) */
#endif
#endif
		}
		if (IsDBCS1(c))
		{					   /* Check if it is a DBC 1st byte (always false at SBCS cfg.) */
			d = (BYTE)p[si++]; /* Get 2nd byte */
			if (!IsDBCS2(d) || i >= ni - 1)
				return FR_INVALID_NAME; /* Reject invalid DBC */
			sfn[i++] = c;
			sfn[i++] = d;
		}
		else
		{ /* SBC */
			if (chk_chr("\"*+,:;<=>\?[]|\x7F", c))
				return FR_INVALID_NAME; /* Reject illegal chrs for SFN */
			if (IsLower(c))
				c -= 0x20; /* To upper */
			sfn[i++] = c;
		}
	}
	*path = p + si; /* Return pointer to the next segment */
	if (i == 0)
		return FR_INVALID_NAME; /* Reject nul string */

	if (sfn[0] == DDEM)
		sfn[0] = RDDEM;						/* If the first character collides with DDEM, replace it with RDDEM */
	sfn[NSFLAG] = (c <= ' ') ? NS_LAST : 0; /* Set last segment flag if end of the path */

	return FR_OK;
#endif /* _USE_LFN != 0 */
	}

	/*-----------------------------------------------------------------------*/
	/* Follow a file path                                                    */
	/*-----------------------------------------------------------------------*/
	struct follow_path_strut {
		DIR *dp;
		const TCHAR *path;
		BYTE ns;
		_FDID *obj;
		FATFS *fs;
	}
	;

	static void follow_path_loop(follow_path_strut *strut) {
		fResult = create_name(strut->dp, &strut->path); /* Get a segment name of the path */
		if (fResult != FR_OK) {
			delete strut;
			callNextCallback();
			return;
		}
		dir_find(strut->dp, [](void *data) {
			follow_path_strut *strut = ((follow_path_strut*) data);
			strut->ns = strut->dp->fn[NSFLAG];
			if (fResult != FR_OK) { /* Failed to find the object */
				if (fResult == FR_NO_FILE) { /* Object is not found */
					if (_NB_FS_RPATH && (strut->ns & NS_DOT)) { /* If dot entry is not exist, stay there */
						if (!(strut->ns & NS_LAST)) {
							follow_path_loop(strut);
							return;
						} /* Continue to follow if not last segment */
						strut->dp->fn[NSFLAG] = NS_NONAME;
						fResult = FR_OK;
					} else { /* Could not find the object */
						if (!(strut->ns & NS_LAST)) {
							fResult = FR_NO_PATH; /* Adjust error code if not last segment */
						}
					}
				}
				delete strut;
				callNextCallback();
				return;
			}
			if (strut->ns & NS_LAST) {
				delete strut;
				callNextCallback();
				return;
			} /* Last segment matched. Function completed. */
			/* Get into the sub-directory */
			if (!(strut->obj->attr & AM_DIRECTORY)) { /* It is not a sub-directory and cannot follow */
				fResult = FR_NO_PATH;
				delete strut;
				callNextCallback();
				return;
			}
#if _NB_FS_EXFAT
			if (strut->fs->fs_type == FS_EXFAT) { /* Save containing directory information for next dirPtr */
				strut->obj->containingDirectoryStartCluster = strut->obj->startCluster;
				strut->obj->sizeOfContainingDirectory = ((DWORD) strut->obj->objSize & 0xFFFFFF00) | strut->obj->stat;
				strut->obj->offsetInContainingDirectory = strut->dp->currentEntryBlockOffset;
				strut->obj->startCluster = ld_dword(strut->fs->dirBuffer + XDIR_FstClus); /* Open next directory */
				strut->obj->stat = strut->fs->dirBuffer[XDIR_GenFlags] & 2;
				strut->obj->objSize = ld_qword(strut->fs->dirBuffer + XDIR_FileSize);
			} else
#endif
			{
				strut->obj->startCluster = ld_clust(strut->fs, strut->fs->win + strut->dp->currentReadWriteOffset % SS(strut->fs)); /* Open next directory */
			}
			follow_path_loop(strut);
		}, strut); /* Find an object with the segment name */
	}

	static void follow_path( /* FR_OK(0): successful, !=0: error code */
	DIR *dp, /* Directory object to return last directory and found object */
	const TCHAR *path, /* Full-path string to find a file or directory */
	void (*callback)(void*), void *data) {
		addCallback(callback, data);
		follow_path_strut *s = new follow_path_strut();
		s->path = path;
		s->dp = dp;
		s->obj = &s->dp->obj;
		s->fs = s->obj->fs;

#if _NB_FS_RPATH != 0
#error
		if (*path != '/' && *path != '\\')
		{									 /* Without heading separator */
			obj->startCluster = s->fs->cdir; /* Start from current directory */
		}
		else
#endif
		{ /* With heading separator */
			while (*s->path == '/' || *s->path == '\\')
				s->path++; /* Strip heading separator */
			s->obj->startCluster = 0; /* Start from root directory */
		}
#if _NB_FS_EXFAT
		s->obj->lastFragSize = 0; /* Invalidate last fragment counter of the object */
#if _NB_FS_RPATH != 0
#error
		if (s->fs->fs_type == FS_EXFAT && obj->startCluster)
		{ /* Retrieve the sub-directory status if needed */
			DIR dj;

			obj->containingDirectoryStartCluster = s->fs->cdc_scl;
			obj->sizeOfContainingDirectory = s->fs->cdc_size;
			obj->offsetInContainingDirectory = s->fs->cdc_ofs;
			fResult = load_obj_dir(&dj, obj);
			if (fResult != FR_OK)
				return res;
			obj->objSize = ld_dword(s->fs->dirBuffer + XDIR_FileSize);
			obj->stat = s->fs->dirBuffer[XDIR_GenFlags] & 2;
		}
#endif
#endif

		if ((UINT) *s->path < ' ') { /* Null path name is the origin directory itself */
			s->dp->fn[NSFLAG] = NS_NONAME;
			dir_setDirectoryIndex(s->dp, 0, [](void *data) {
				follow_path_strut *strut = ((follow_path_strut*) data);
				delete strut;
				callNextCallback();
			}, s);
			return;
		} else { /* Follow path */
			follow_path_loop(s);
		}

		// return res;
	}

	/*-----------------------------------------------------------------------*/
	/* Get logical drive number from path name                               */
	/*-----------------------------------------------------------------------*/

	static int get_ldnumber( /* Returns logical drive number (-1:invalid drive) */
	const TCHAR **path /* Pointer to pointer to the path name */
	) {
		const TCHAR *tp, *tt;
		UINT i;
		int vol = -1;
#if _NB_STR_VOLUME_ID /* Find string drive id */
#error
		static const char *const volid[] = {_NB_VOLUME_STRS};
		const char *sp;
		char c;
		TCHAR tc;
#endif

		if (*path) { /* If the pointer is not a null */
			for (tt = *path; (UINT) *tt >= (_NB_USE_LFN ? ' ' : '!') && *tt != ':'; tt++)
				; /* Find ':' in the path */
			if (*tt == ':') { /* If a ':' is exist in the path name */
				tp = *path;
				i = *tp++ - '0';
				if (i < 10 && tp == tt) { /* Is there a numeric drive id? */
					if (i < _NB_VOLUMES) { /* If a drive id is found, get the value and strip it */
						vol = (int) i;
						*path = ++tt;
					}
				}
#if _NB_STR_VOLUME_ID
#error
				else
				{ /* No numeric drive number, find string drive id */
					i = 0;
					tt++;
					do
					{
						sp = volid[i];
						tp = *path;
						do
						{ /* Compare a string drive id with path name */
							c = *sp++;
							tc = *tp++;
							if (IsLower(tc))
								tc -= 0x20;
						} while (c && (TCHAR)c == tc);
					} while ((c || tp != tt) && ++i < _NB_VOLUMES); /* Repeat for each id until pattern match */
					if (i < _NB_VOLUMES)
					{ /* If a drive id is found, get the value and strip it */
						vol = (int)i;
						*path = tt;
					}
				}
#endif
				return vol;
			}
#if _NB_FS_RPATH != 0 && _NB_VOLUMES >= 2
			vol = CurrVol; /* Current drive */
#else
			vol = 0; /* Drive 0 */
#endif
		}
		return vol;
	}

	/*-----------------------------------------------------------------------*/
	/* Load a sector and check if it is an FAT boot sector                   */
	/*-----------------------------------------------------------------------*/
	struct check_fs_strut {
		FATFS *fs;
		DWORD sect;
	}
	;

	static void check_fs(/* 0:FAT, 1:exFAT, 2:Valid BS but not FAT, 3:Not a BS, 4:Disk error */
	FATFS *fs, DWORD sectInput, /* Sector# (lba) to load and check if it is an FAT-VBR or not */
	void (*callback)(void*), void *data) {
		check_fs_strut *s = new check_fs_strut();
		s->fs = fs;
		s->fs->wflag = 0;
		s->fs->winSector = 0xFFFFFFFF; /* Invaidate window */
		s->sect = sectInput;
		addCallback(callback, data);

		move_window(s->fs, s->sect, [](void *data) {
			check_fs_strut *strut = ((check_fs_strut*) data);
			if (fResult != FR_OK) {
				checkFS_Value = 4;
				delete strut;
				callNextCallback();
				return;
			}
			if (ld_word(strut->fs->win + BS_55AA) != 0xAA55) {
				checkFS_Value = 3;
				delete strut;
				callNextCallback();
				return;
			} /* Check boot record signature (always placed here even if the sector size is >512) */
			if (strut->fs->win[BS_JmpBoot] == 0xE9 || (strut->fs->win[BS_JmpBoot] == 0xEB && strut->fs->win[BS_JmpBoot + 2] == 0x90)) {
				if ((ld_dword(strut->fs->win + BS_FilSysType) & 0xFFFFFF) == 0x544146) {
					checkFS_Value = 0;
					delete strut;
					callNextCallback();
					return;
				} /* Check "FAT" string */
				if (ld_dword(strut->fs->win + BS_FilSysType32) == 0x33544146) {
					checkFS_Value = 0;
					delete strut;
					callNextCallback();
					return;
				} /* Check "FAT3" string */
			}
#if _NB_FS_EXFAT
			if (!mem_cmp(strut->fs->win + BS_JmpBoot, "\xEB\x76\x90"
					"EXFAT   ", 11)) {
				checkFS_Value = 1;
				delete strut;
				callNextCallback();
				return;
			}
#endif
			checkFS_Value = 2;
			delete strut;
			callNextCallback();
			return;
		}, s);
	}

	/*-----------------------------------------------------------------------*/
	/* Find logical drive and check if the volume is mounted                 */
	/*-----------------------------------------------------------------------*/
	struct find_volume_strut {
		const TCHAR **path;
		FATFS **rfs;
		BYTE mode;
		BYTE fmt;
		BYTE *pt;
		int vol;
		DSTATUS stat;
		DWORD bsect;
		DWORD fasize;
		DWORD tsect;
		DWORD sysect;
		DWORD nclst;
		DWORD szbfat;
		DWORD br[4];
		WORD nrsv;
		FATFS *fs;
		UINT i;
	}
	;

	static void find_volume_end(find_volume_strut *strut) {
		strut->fs->fs_type = strut->fmt; /* FAT sub-type */
		strut->fs->id = ++Fsid; /* File system mount ID */
#if _NB_USE_LFN == 1
		strut->fs->lfnBuffer = LfnBuf; /* Static LFN working buffer */
#if _NB_FS_EXFAT
		strut->fs->dirBuffer = DirBuf; /* Static directory block scratchpad buuffer */
#endif
#endif
#if _NB_FS_RPATH != 0
#error
		strut->fs->cdir = 0; /* Initialize current directory */
#endif
#if _NB_FS_LOCK != 0 /* Clear file lock semaphores */
		clear_lock(strut->fs);
#endif
		fResult = FR_OK;
		delete strut;
		callNextCallback();
		return;
	}

	static void find_volume_a(find_volume_strut *strut) {
		if (strut->fmt == 4) {
			fResult = FR_DISK_ERROR;
			delete strut;
			callNextCallback();
			return;
		} /* An error occured in the disk I/O layer */
		if (strut->fmt >= 2) {
			fResult = FR_NO_FILESYSTEM;
			delete strut;
			callNextCallback();
			return;
		} /* No FAT volume is found */

		/* An FAT volume is found (bsect). Following code initializes the file system object */

#if _NB_FS_EXFAT
		if (strut->fmt == 1) {
			QWORD maxlba;

			for (strut->i = BPB_ZeroedEx; strut->i < BPB_ZeroedEx + 53 && strut->fs->win[strut->i] == 0; strut->i++)
				; /* Check zero filler */
			if (strut->i < BPB_ZeroedEx + 53) {
				fResult = FR_NO_FILESYSTEM;
				delete strut;
				callNextCallback();
				return;
			}

			if (ld_word(strut->fs->win + BPB_FSVerEx) != 0x100) {
				fResult = FR_NO_FILESYSTEM;
				delete strut;
				callNextCallback();
				return;
			} /* Check exFAT revision (Must be 1.0) */

			if (1 << strut->fs->win[BPB_BytsPerSecEx] != SS(strut->fs)) { /* (BPB_BytsPerSecEx must be equal to the physical sector size) */
				fResult = FR_NO_FILESYSTEM;
				delete strut;
				callNextCallback();
				return;
			}

			maxlba = ld_qword(strut->fs->win + BPB_TotSecEx) + strut->bsect; /* Last LBA + 1 of the volume */
			if (maxlba >= 0x100000000) {
				fResult = FR_NO_FILESYSTEM;
				delete strut;
				callNextCallback();
				return;
			} /* (It cannot be handled in 32-bit LBA) */

			strut->fs->fatSectorSize = ld_dword(strut->fs->win + BPB_FatSzEx); /* Number of sectors per FAT */

			strut->fs->n_fats = strut->fs->win[BPB_NumFATsEx]; /* Number of FATs */
			if (strut->fs->n_fats != 1) {
				fResult = FR_NO_FILESYSTEM;
				delete strut;
				callNextCallback();
				return;
			} /* (Supports only 1 FAT) */

			strut->fs->clusterSize = 1 << strut->fs->win[BPB_SecPerClusEx]; /* Cluster size */
			if (strut->fs->clusterSize == 0) {
				fResult = FR_NO_FILESYSTEM;
				delete strut;
				callNextCallback();
				return;
			} /* (Must be 1..32768) */

			strut->nclst = ld_dword(strut->fs->win + BPB_NumClusEx); /* Number of clusters */
			if (strut->nclst > MAX_EXFAT) {
				fResult = FR_NO_FILESYSTEM;
				delete strut;
				callNextCallback();
				return;
			} /* (Too many clusters) */
			strut->fs->numberOfFatEntries = strut->nclst + 2;

			/* Boundaries and Limits */
			strut->fs->volBaseSector = strut->bsect;
			strut->fs->dataBaseSector = strut->bsect + ld_dword(strut->fs->win + BPB_DataOfsEx);
			strut->fs->fatBaseSector = strut->bsect + ld_dword(strut->fs->win + BPB_FatOfsEx);
			if (maxlba < (QWORD) strut->fs->dataBaseSector + strut->nclst * strut->fs->clusterSize) {
				fResult = FR_NO_FILESYSTEM;
				delete strut;
				callNextCallback();
				return;
			} /* (Volume size must not be smaller than the size requiered) */
			strut->fs->dirBaseSector = ld_dword(strut->fs->win + BPB_RootClusEx);

			/* Check if bitmap location is in assumption (at the first cluster) */
			move_window(strut->fs, clust2sect(strut->fs, strut->fs->dirBaseSector), [](void *data) {
				find_volume_strut *strut = ((find_volume_strut*) data);
				if (fResult != FR_OK) {
					fResult = FR_DISK_ERROR;
					delete strut;
					callNextCallback();
					return;
				}
				for (strut->i = 0; strut->i < SS(strut->fs); strut->i +=
				SZDIRE)
				{
					if (strut->fs->win[strut->i] == 0x81 && ld_dword(strut->fs->win + strut->i + 20) == 2)
						break; /* 81 entry with cluster #2? */
				}
				if (strut->i == SS(strut->fs)) {
					fResult = FR_NO_FILESYSTEM;
					delete strut;
					callNextCallback();
					return;
				}
#if !_NB_FS_READONLY
				strut->fs->lastCluster = strut->fs->numberOfFreeClusters = 0xFFFFFFFF; /* Initialize cluster allocation information */
#endif
				strut->fmt = FS_EXFAT; /* FAT sub-type */
				find_volume_end(strut);
			}, strut);
			return;
		} else
#endif /* _NB_FS_EXFAT */
		{
			if (ld_word(strut->fs->win + BPB_BytsPerSec) != SS(strut->fs)) {
				fResult = FR_NO_FILESYSTEM;
				delete strut;
				callNextCallback();
				return;
			} /* (BPB_BytsPerSec must be equal to the physical sector size) */

			strut->fasize = ld_word(strut->fs->win + BPB_FATSz16); /* Number of sectors per FAT */
			if (strut->fasize == 0) {
				strut->fasize = ld_dword(strut->fs->win + BPB_FATSz32);
			}
			strut->fs->fatSectorSize = strut->fasize;

			strut->fs->n_fats = strut->fs->win[BPB_NumFATs]; /* Number of FATs */
			if (strut->fs->n_fats != 1 && strut->fs->n_fats != 2) {
				fResult = FR_NO_FILESYSTEM;
				delete strut;
				callNextCallback();
				return;
			} /* (Must be 1 or 2) */
			strut->fasize *= strut->fs->n_fats; /* Number of sectors for FAT area */

			strut->fs->clusterSize = strut->fs->win[BPB_SecPerClus]; /* Cluster size */
			if (strut->fs->clusterSize == 0 || (strut->fs->clusterSize & (strut->fs->clusterSize - 1))) {
				fResult = FR_NO_FILESYSTEM;
				delete strut;
				callNextCallback();
				return;
			} /* (Must be power of 2) */

			strut->fs->numberOfRootDirectoryEntries = ld_word(strut->fs->win + BPB_RootEntCnt); /* Number of root directory entries */
			if (strut->fs->numberOfRootDirectoryEntries % (SS(strut->fs) / SZDIRE)) {
				fResult = FR_NO_FILESYSTEM;
				delete strut;
				callNextCallback();
				return;
			} /* (Must be sector aligned) */

			strut->tsect = ld_word(strut->fs->win + BPB_TotSec16); /* Number of sectors on the volume */
			if (strut->tsect == 0) {
				strut->tsect = ld_dword(strut->fs->win + BPB_TotSec32);
			}

			strut->nrsv = ld_word(strut->fs->win + BPB_RsvdSecCnt); /* Number of reserved sectors */
			if (strut->nrsv == 0) {
				fResult = FR_NO_FILESYSTEM;
				delete strut;
				callNextCallback();
				return;
			} /* (Must not be 0) */

			/* Determine the FAT sub type */
			strut->sysect = strut->nrsv + strut->fasize + strut->fs->numberOfRootDirectoryEntries / (SS(strut->fs) / SZDIRE); /* RSV + FAT + DIR */
			if (strut->tsect < strut->sysect) {
				fResult = FR_NO_FILESYSTEM;
				delete strut;
				callNextCallback();
				return;
			} /* (Invalid volume size) */
			strut->nclst = (strut->tsect - strut->sysect) / strut->fs->clusterSize; /* Number of clusters */
			if (strut->nclst == 0) {
				fResult = FR_NO_FILESYSTEM;
				delete strut;
				callNextCallback();
				return;
			} /* (Invalid volume size) */
			strut->fmt = FS_FAT32;
			if (strut->nclst <= MAX_FAT16) {
				strut->fmt = FS_FAT16;
			}
			if (strut->nclst <= MAX_FAT12) {
				strut->fmt = FS_FAT12;
			}

			/* Boundaries and Limits */
			strut->fs->numberOfFatEntries = strut->nclst + 2; /* Number of FAT entries */
			strut->fs->volBaseSector = strut->bsect; /* Volume start sector */
			strut->fs->fatBaseSector = strut->bsect + strut->nrsv; /* FAT start sector */
			strut->fs->dataBaseSector = strut->bsect + strut->sysect; /* Data start sector */
			if (strut->fmt == FS_FAT32) {
				if (ld_word(strut->fs->win + BPB_FSVer32) != 0) {
					fResult = FR_NO_FILESYSTEM;
					delete strut;
					callNextCallback();
					return;
				} /* (Must be FAT32 revision 0.0) */
				if (strut->fs->numberOfRootDirectoryEntries) {
					fResult = FR_NO_FILESYSTEM;
					delete strut;
					callNextCallback();
					return;
				} /* (BPB_RootEntCnt must be 0) */
				strut->fs->dirBaseSector = ld_dword(strut->fs->win + BPB_RootClus32); /* Root directory start cluster */
				strut->szbfat = strut->fs->numberOfFatEntries * 4; /* (Needed FAT size) */
			} else {
				if (strut->fs->numberOfRootDirectoryEntries == 0) {
					fResult = FR_NO_FILESYSTEM;
					delete strut;
					callNextCallback();
					return;
				} /* (BPB_RootEntCnt must not be 0) */
				strut->fs->dirBaseSector = strut->fs->fatBaseSector + strut->fasize; /* Root directory start sector */
				strut->szbfat = (strut->fmt == FS_FAT16) ? /* (Needed FAT size) */
				strut->fs->numberOfFatEntries * 2 : strut->fs->numberOfFatEntries * 3 / 2 + (strut->fs->numberOfFatEntries & 1);
			}
			if (strut->fs->fatSectorSize < (strut->szbfat + (SS(strut->fs) - 1)) / SS(strut->fs)) {
				fResult = FR_NO_FILESYSTEM;
				delete strut;
				callNextCallback();
				return;
			} /* (BPB_FATSz must not be less than the size needed) */

#if !_NB_FS_READONLY
			/* Get FSINFO if available */
			strut->fs->lastCluster = strut->fs->numberOfFreeClusters = 0xFFFFFFFF; /* Initialize cluster allocation information */
			strut->fs->fsi_flag = 0x80;
#if (_NB_FS_NOFSINFO & 3) != 3
			move_window(strut->fs, strut->bsect + 1, [](void *data) {
				find_volume_strut *strut = ((find_volume_strut*) data);
				if (strut->fmt == FS_FAT32 /* Enable FSINFO only if FAT32 and BPB_FSInfo32 == 1 */
				&& ld_word(strut->fs->win + BPB_FSInfo32) == 1 && fResult == FR_OK) {
					strut->fs->fsi_flag = 0;
					if (ld_word(strut->fs->win + BS_55AA) == 0xAA55 /* Load FSINFO data if available */
					&& ld_dword(strut->fs->win + FSI_LeadSig) == 0x41615252 && ld_dword(strut->fs->win + FSI_StrucSig) == 0x61417272) {
#if (_NB_FS_NOFSINFO & 1) == 0
				strut->fs->numberOfFreeClusters = ld_dword(strut->fs->win + FSI_Free_Count);
#endif
#if (_NB_FS_NOFSINFO & 2) == 0
				strut->fs->lastCluster = ld_dword(strut->fs->win + FSI_Nxt_Free);
#endif
			}
		}
		find_volume_end(strut);
	}		, strut);
			return;
#endif /* (_FS_NOFSINFO & 3) != 3 */
#endif /* !_FS_READONLY */
		}
		find_volume_end(strut);
	}

	static void find_volume_loop(find_volume_strut *strut) {
		/* Find an FAT volume */
		strut->bsect = strut->br[strut->i];
		if (strut->bsect) {
			check_fs(strut->fs, strut->bsect, [](void *data) {
				find_volume_strut *strut = ((find_volume_strut*) data);
				strut->fmt = checkFS_Value;
				if (LD2PT(strut->vol) == 0 && strut->fmt >= 2 && ++strut->i < 4) {
					find_volume_loop(strut);
					return;
				} else {
					find_volume_a(strut);
					return;
				}
			}, strut);
			return;
		} else {
			strut->fmt = 3;
		} /* Check the partition */
		if (LD2PT(strut->vol) == 0 && strut->fmt >= 2 && ++strut->i < 4) {
			find_volume_loop(strut);
			return;
		} else {
			find_volume_a(strut);
			return;
		}
	}

	static void find_volume_c(find_volume_strut *strut) {
		/* Find an FAT partition on the drive. Supports only generic partitioning rules, FDISK and SFD. */
		strut->bsect = 0;
		check_fs(strut->fs, strut->bsect, [](void *data) {
			find_volume_strut *strut = ((find_volume_strut*) data);
			strut->fmt = checkFS_Value;
			if (strut->fmt == 2 || (strut->fmt < 2 && LD2PT(strut->vol) != 0)) { /* Not an FAT-VBR or forced partition number */
				for (strut->i = 0; strut->i < 4; strut->i++) { /* Get partition offset */
					strut->pt = strut->fs->win + (MBR_Table + strut->i * SZ_PTE);
					strut->br[strut->i] = strut->pt[PTE_System] ? ld_dword(strut->pt + PTE_StLba) : 0;
				}
				strut->i = LD2PT(strut->vol); /* Partition number: 0:auto, 1-4:forced */
				if (strut->i) {
					strut->i--;
				}
				find_volume_loop(strut);
				return;
			} else {
				find_volume_a(strut);
				return;
			}
		}, strut); /* Load sector 0 and check if it is an FAT-VBR as SFD */
	}

	static void find_volume_b(find_volume_strut *strut) {
		/* The file system object is not valid. */
		/* Following code attempts to mount the volume. (analyze BPB and initialize the fs object) */

		strut->fs->fs_type = 0; /* Clear the file system object */
		strut->fs->driveNumber = LD2PD(strut->vol); /* Bind the logical drive and a physical drive */

#if _NB_MAX_SS != _NB_MIN_SS /* Get sector size (multiple sector size cfg only) */
		if (disk_ioctl(strut->fs->driveNumber, GET_SECTOR_SIZE, &SS(strut->fs)) != RES_OK)
		{
			// return FR_DISK_ERROR;
			fResult = FR_DISK_ERROR;
			delete strut;
			callNextCallback();
			return;
		}
		if (SS(strut->fs) > _NB_MAX_SS || SS(strut->fs) < _NB_MIN_SS || (SS(strut->fs) & (SS(strut->fs) - 1)))
		{
			// return FR_DISK_ERROR;
			fResult = FR_DISK_ERROR;
			delete strut;
			callNextCallback();
			return;
		}
#endif
		find_volume_c(strut);
		return;
	}

	static void find_volume( /* FR_OK(0): successful, !=0: any error occurred */
	const TCHAR **path, /* Pointer to pointer to the path name (drive number) */
	FATFS **rfs, /* Pointer to pointer to the found file system object */
	BYTE mode, /* !=0: Check write protection for write access */
	void (*callback)(void*), void *data) {
		addCallback(callback, data);

		/* Get logical drive number */
		find_volume_strut *s = new find_volume_strut();
		s->path = path;
		s->rfs = rfs;
		s->mode = mode;

		*s->rfs = 0;
		s->vol = get_ldnumber(s->path);
		if (s->vol < 0) {
			fResult = FR_INVALID_DRIVE;
			delete s;
			callNextCallback();
			return;
		}

		/* Check if the file system object is valid or not */
		s->fs = FatFs[s->vol]; /* Get pointer to the file system object */
		if (!s->fs) {
			fResult = FR_NOT_ENABLED;
			delete s;
			callNextCallback();
			return;
		} /* Is the file system object available? */

		ENTER_FF(s->fs); /* Lock the volume */
		*s->rfs = s->fs; /* Return pointer to the file system object */

		s->mode &= (BYTE) ~FA_READ; /* Desired access mode, write access or not */
		if (s->fs->fs_type) { /* If the volume has been mounted */
			voidPtr = s;
			disk_status(s->fs->driveNumber, [](DSTATUS dStat) {
				find_volume_strut *strut = (find_volume_strut*) voidPtr;
				strut->stat = dStat;
				if (!(strut->stat & STA_NOT_INITIALIZED)) { /* and the physical drive is kept initialized */
					if (!_NB_FS_READONLY && strut->mode && (strut->stat & STA_WRITE_PROTECTED)) { /* Check write protection if needed */
						fResult = FR_WRITE_PROTECTED;
						delete strut;
						callNextCallback();
						return;
					}/* The file system object is valid */
					fResult = FR_OK;
					delete strut;
					callNextCallback();
					return;
				}
				find_volume_b(strut);
				return;
			});
			return;
		}
		find_volume_b(s);
		return;
	}

	/*-----------------------------------------------------------------------*/
	/* Check if the file/directory object is valid or not                    */
	/*-----------------------------------------------------------------------*/
	struct validate_strut {
		_FDID *obj;
		FATFS **fs;
	}
	;

	static void validate_a(validate_strut *strut) {
		if (fResult == FR_OK) {
			*strut->fs = strut->obj->fs;
		} else {
			*strut->fs = 0;
		} /* Corresponding filesystem object */
		delete strut;
		callNextCallback();
		return;
	}

	static void validate( /* Returns FR_OK or FR_INVALID_OBJECT */
	_FDID *obj, /* Pointer to the _OBJ, the 1st member in the FIL/DIR object, to check validity */
	FATFS **fs, /* Pointer to pointer to the owner file system object to return */
	void (*callback)(void*), void *data) {
		addCallback(callback, data);
		validate_strut *s = new validate_strut();
		fResult = FR_INVALID_OBJECT;
		s->obj = obj;
		s->fs = fs;

		if (s->obj && s->obj->fs && s->obj->fs->fs_type && s->obj->id == s->obj->fs->id) { /* Test if the object is valid */
#if _NB_FS_REENTRANT
#error
			if (lock_fs(obj->fs))
			{ /* Obtain the filesystem object */
				if (!(disk_status(obj->fs->driveNumber) & STA_NOT_INITIALIZED))
				{ /* Test if the phsical drive is kept initialized */
					res = FR_OK;
				}
				else
				{
					unlock_fs(obj->fs, FR_OK);
				}
			}
			else
			{
				res = FR_TIMEOUT;
			}
#else
			voidPtr = s;
			disk_status(s->obj->fs->driveNumber, [](DSTATUS dStat) {
				validate_strut *strut = (validate_strut*) voidPtr;
				if (!(dStat & STA_NOT_INITIALIZED)) { /* Test if the phsical drive is kept initialized */
					fResult = FR_OK;
					validate_a(strut);
					return;
				} else {
					validate_a(strut);
					return;
				}
			});
			return;
#endif
			validate_a(s);
			return;
		}
		validate_a(s);
		return;
	}

	/*---------------------------------------------------------------------------

	 Public Functions (FatFs API)

	 ----------------------------------------------------------------------------*/

	/*-----------------------------------------------------------------------*/
	/* Mount/Unmount a Logical Drive                                         */
	/*-----------------------------------------------------------------------*/
	struct f_mount_strut {
		FATFS *fs; /* Pointer to the file system object (NULL:unmount)*/
		const TCHAR *path; /* Logical drive number to be mounted/unmounted */
		BYTE opt;
		FATFS *cfs;
		int vol;
		const TCHAR *rp;
	}
	;

	void f_mount(FATFS *fs, /* Pointer to the file system object (NULL:unmount)*/
	const TCHAR *path, /* Logical drive number to be mounted/unmounted */
	BYTE opt, /* Mode option 0:Do not mount (delayed mount), 1:Mount immediately */
	void (*callback)(void*)) {
		addCallback(callback, &fResult);
		f_mount_strut *s = new f_mount_strut();
		s->rp = path;
		s->fs = fs;
		s->path = path;
		s->opt = opt;

		/* Get logical drive number */
		s->vol = get_ldnumber(&s->rp);
		if (s->vol < 0) {
			fResult = FR_INVALID_DRIVE;
			delete s;
			callNextCallback();
			return;
		}
		s->cfs = FatFs[s->vol]; /* Pointer to fs object */

		if (s->cfs) {
#if _NB_FS_LOCK != 0
			clear_lock(s->cfs);
#endif
#if _NB_FS_REENTRANT /* Discard sync object of the current volume */
			if (!ff_del_syncobj(s->cfs->sobj))
				return FR_INT_ERROR;
#endif
			s->cfs->fs_type = 0; /* Clear old fs object */
		}

		if (s->fs) {
			s->fs->fs_type = 0; /* Clear new fs object */
#if _NB_FS_REENTRANT			/* Create sync object for the new volume */
#error
			if (!ff_cre_syncobj((BYTE)vol, &fs->sobj))
				return FR_INT_ERROR;
#endif
		}
		FatFs[s->vol] = s->fs; /* Register new fs object */

		if (!s->fs || s->opt != 1) {
			fResult = FR_OK;
			delete s;
			callNextCallback();
			return;
		} /* Do not mount now, it will be mounted later */

		find_volume(&s->path, &s->fs, 0, [](void *data) {
			f_mount_strut *strut = ((f_mount_strut*) data);
			delete strut;
			callNextCallback();
			return;
		}, s);
		return; /* Force mounted the volume */
		// LEAVE_FF(fs, res);
	}

	/*-----------------------------------------------------------------------*/
	/* Open or Create a File                                                 */
	/*-----------------------------------------------------------------------*/
	struct f_open_strut {
		FIL *fp; /* Pointer to the blank file object */
		const TCHAR *path; /* Pointer to the file name */
		BYTE mode; /* Access mode and file open mode flags */
		DIR dj;
		FATFS *fs;
#if !_FS_READONLY
		DWORD dw, cl, bcs, clst, sc;
		FSIZE_t ofs;
#endif
	}
	;

	void f_open_loop_end(f_open_strut *strut) {
		strut->fp->currentFilePtrCluster = strut->clst;
		if (fResult == FR_OK && strut->ofs % SS(strut->fs)) { /* Fill sector buffer if not on the sector boundary */
			if ((strut->sc = clust2sect(strut->fs, strut->clst)) == 0) {
				fResult = FR_INT_ERROR;
			} else {
				strut->fp->sectorNumber = strut->sc + (DWORD) (strut->ofs / SS(strut->fs));
#if !_NB_FS_TINY
				voidPtr = strut;
				disk_read(strut->fs->driveNumber, strut->fp->buf, strut->fp->sectorNumber, 1, [](DRESULT dRes) {
					f_open_strut *strut = (f_open_strut*) voidPtr;
					if (dRes != RES_OK) {
						fResult = FR_DISK_ERROR;
					}FREE_NAMBUF();
					if (fResult != FR_OK) {
						strut->fp->obj.fs = 0; /* Invalidate file object on error */
					}
					delete strut;
					callNextCallback();
					return;
				});
				return;
#endif
			}
		}FREE_NAMBUF();
		if (fResult != FR_OK) {
			strut->fp->obj.fs = 0; /* Invalidate file object on error */
		}
		delete strut;
		callNextCallback();
		return;
	}

	void f_open_loop(f_open_strut *strut) {
		if (fResult == FR_OK && strut->ofs > strut->bcs) {
			get_fat(&strut->fp->obj, strut->clst, [](void *data) {
				f_open_strut *strut = ((f_open_strut*) data);
				strut->clst = getFatValue;
				if (strut->clst <= 1) {
					fResult = FR_INT_ERROR;
				}
				if (strut->clst == 0xFFFFFFFF) {
					fResult = FR_DISK_ERROR;
				}
				strut->ofs -= strut->bcs;
				f_open_loop(strut);
				return;
			}, strut);
			return;
		}
		f_open_loop_end(strut);
		return;
	}

	void f_open_a(f_open_strut *strut) {

#if !_NB_FS_READONLY /* R/W configuration */
		if (fResult == FR_OK) {
			if (strut->mode & FA_CREATE_ALWAYS) /* Set file change flag if created or overwritten */
				strut->mode |= FA_MODIFIED;
			strut->fp->dirSectNumber = strut->fs->winSector; /* Pointer to the directory entry */
			strut->fp->dirEntryPtr = strut->dj.dirPtr;
#if _NB_FS_LOCK != 0
			strut->fp->obj.lockid = inc_lock(&strut->dj, (strut->mode & ~FA_READ) ? 1 : 0);
			if (!strut->fp->obj.lockid) {
				fResult = FR_INT_ERROR;
			}
#endif
		}
#else /* R/O configuration */
#error
	if (fResult == FR_OK)
	{
		if (dj.fn[NSFLAG] & NS_NONAME)
		{ /* Origin directory itself? */
			fResult = FR_INVALID_NAME;
		}
		else
		{
			if (dj.obj.attr & AM_DIRECTORY)
			{ /* It is a directory */
				fResult = FR_NO_FILE;
			}
		}
	}
#endif

		if (fResult == FR_OK) {
#if _NB_FS_EXFAT
			if (strut->fs->fs_type == FS_EXFAT) {
				strut->fp->obj.containingDirectoryStartCluster = strut->dj.obj.startCluster; /* Get containing directory info */
				strut->fp->obj.sizeOfContainingDirectory = ((DWORD) strut->dj.obj.objSize & 0xFFFFFF00) | strut->dj.obj.stat;
				strut->fp->obj.offsetInContainingDirectory = strut->dj.currentEntryBlockOffset;
				strut->fp->obj.startCluster = ld_dword(strut->fs->dirBuffer + XDIR_FstClus); /* Get object allocation info */
				strut->fp->obj.objSize = ld_qword(strut->fs->dirBuffer + XDIR_FileSize);
				strut->fp->obj.stat = strut->fs->dirBuffer[XDIR_GenFlags] & 2;
			} else
#endif
			{
				strut->fp->obj.startCluster = ld_clust(strut->fs, strut->dj.dirPtr); /* Get object allocation info */
				strut->fp->obj.objSize = ld_dword(strut->dj.dirPtr + DIR_FileSize);
			}
#if _NB_USE_FASTSEEK
			strut->fp->clusterLinkMapTablePtr = 0; /* Disable fast seek mode */
#endif
			strut->fp->obj.fs = strut->fs; /* Validate the file object */
			strut->fp->obj.id = strut->fs->id;
			strut->fp->flag = strut->mode; /* Set file access mode */
			strut->fp->err = 0; /* Clear error flag */
			strut->fp->sectorNumber = 0; /* Invalidate current data sector */
			strut->fp->fileReadWritePtr = 0; /* Set file pointer top of the file */
#if !_NB_FS_READONLY
#if !_NB_FS_TINY
			mem_set(strut->fp->buf, 0, _NB_MAX_SS); /* Clear sector buffer */
#endif
			if ((strut->mode & FA_SEEKEND) && strut->fp->obj.objSize > 0) { /* Seek to end of file if FA_OPEN_APPEND is specified */
				strut->fp->fileReadWritePtr = strut->fp->obj.objSize; /* Offset to seek */
				strut->bcs = (DWORD) strut->fs->clusterSize * SS(strut->fs); /* Cluster size in byte */
				strut->clst = strut->fp->obj.startCluster; /* Follow the cluster chain */
				strut->ofs = strut->fp->obj.objSize;
				f_open_loop(strut);
				return;
			}
#endif
		}

		FREE_NAMBUF();
		if (fResult != FR_OK) {
			strut->fp->obj.fs = 0; /* Invalidate file object on error */
		}
		delete strut;
		callNextCallback();
		return;
	}

	void f_open_b(f_open_strut *strut) {
		if (fResult == FR_OK && (strut->mode & FA_CREATE_ALWAYS)) { /* Truncate it if overwrite mode */
			strut->dw = GET_FATTIME();
#if _NB_FS_EXFAT
			if (strut->fs->fs_type == FS_EXFAT) {
				/* Get current allocation info */
				strut->fp->obj.fs = strut->fs;
				strut->fp->obj.startCluster = ld_dword(strut->fs->dirBuffer + XDIR_FstClus);
				strut->fp->obj.objSize = ld_qword(strut->fs->dirBuffer + XDIR_FileSize);
				strut->fp->obj.stat = strut->fs->dirBuffer[XDIR_GenFlags] & 2;
				strut->fp->obj.lastFragSize = 0;
				/* Initialize directory entry block */
				st_dword(strut->fs->dirBuffer + XDIR_CrtTime, strut->dw); /* Set created time */
				strut->fs->dirBuffer[XDIR_CrtTime10] = 0;
				st_dword(strut->fs->dirBuffer + XDIR_ModTime, strut->dw); /* Set modified time */
				strut->fs->dirBuffer[XDIR_ModTime10] = 0;
				strut->fs->dirBuffer[XDIR_Attr] = AM_ARCHIVE; /* Reset attribute */
				st_dword(strut->fs->dirBuffer + XDIR_FstClus, 0); /* Reset file allocation info */
				st_qword(strut->fs->dirBuffer + XDIR_FileSize, 0);
				st_qword(strut->fs->dirBuffer + XDIR_ValidFileSize, 0);
				strut->fs->dirBuffer[XDIR_GenFlags] = 1;
				store_xdir(&strut->dj, [](void *data) {
					f_open_strut *strut = ((f_open_strut*) data);
					if (fResult == FR_OK && strut->fp->obj.startCluster) { /* Remove the cluster chain if exist */
						remove_chain(&strut->fp->obj, strut->fp->obj.startCluster, 0, [](void *data) {
							f_open_strut *strut = ((f_open_strut*) data);
							strut->fs->lastCluster = strut->fp->obj.startCluster - 1; /* Reuse the cluster hole */
							f_open_a(strut);
							return;
						},strut);
						return;
					}
					f_open_a(strut);
					return;
				}, strut);
				return;
			} else
#endif
			{
				/* Clean directory info */
				st_dword(strut->dj.dirPtr + DIR_CrtTime, strut->dw); /* Set created time */
				st_dword(strut->dj.dirPtr + DIR_ModTime, strut->dw); /* Set modified time */
				strut->dj.dirPtr[DIR_Attr] = AM_ARCHIVE; /* Reset attribute */
				strut->cl = ld_clust(strut->fs, strut->dj.dirPtr); /* Get cluster chain */
				st_clust(strut->fs, strut->dj.dirPtr, 0); /* Reset file allocation info */
				st_dword(strut->dj.dirPtr + DIR_FileSize, 0);
				strut->fs->wflag = 1;

				if (strut->cl) { /* Remove the cluster chain if exist */
					strut->dw = strut->fs->winSector;
					remove_chain(&strut->dj.obj, strut->cl, 0, [](void *data) {
						f_open_strut *strut = ((f_open_strut*) data);
						if (fResult == FR_OK) {
							move_window(strut->fs, strut->dw, [](void *data) {
								f_open_strut *strut = ((f_open_strut*) data);
								strut->fs->lastCluster = strut->cl - 1; /* Reuse the cluster hole */
								f_open_a(strut);
								return;
							},strut);
							return;
						}
						f_open_a(strut);
						return;
					}, strut);
					return;
				}
			}
		}
		f_open_a(strut);
		return;
	}

	void f_open(FIL *fpi, /* Pointer to the blank file object */
	const TCHAR *pathi, /* Pointer to the file name */
	BYTE modei /* Access mode and file open mode flags */
	, void (*callback)(void*)) {
		addCallback(callback, &fResult);
		f_open_strut *s = new f_open_strut();
		s->fp = fpi;
		s->path = pathi;
		s->mode = modei;

		DEF_NAMBUF

		if (!s->fp) {
			fResult = FR_INVALID_OBJECT;
			delete s;
			callNextCallback();
			return;
		}

		/* Get logical drive */
		s->mode &=
		_NB_FS_READONLY ?
		FA_READ :
							FA_READ | FA_WRITE | FA_CREATE_ALWAYS | FA_CREATE_NEW | FA_OPEN_ALWAYS | FA_OPEN_APPEND | FA_SEEKEND;
		find_volume(&s->path, &s->fs, s->mode, [](void *data) {
			f_open_strut *strut = ((f_open_strut*) data);

			if (fResult == FR_OK) {
				strut->dj.obj.fs = strut->fs;
				INIT_NAMBUF(strut->fs);
				follow_path(&strut->dj, strut->path, [](void *data) {
					f_open_strut *strut = ((f_open_strut*) data);
#if !_NB_FS_READONLY /* R/W configuration */
					if (fResult == FR_OK) {
						if (strut->dj.fn[NSFLAG] & NS_NONAME) { /* Origin directory itself? */
							fResult = FR_INVALID_NAME;
						}
#if _NB_FS_LOCK != 0
					else {
						fResult = chk_lock(&strut->dj, (strut->mode & ~FA_READ) ? 1 : 0);
					}
#endif
					}
					/* Create or Open a file */
					if (strut->mode & (FA_CREATE_ALWAYS | FA_OPEN_ALWAYS | FA_CREATE_NEW)) {
						if (fResult != FR_OK) { /* No file, create new */
							if (fResult == FR_NO_FILE) { /* There is no file to open, create a new entry */
#if _NB_FS_LOCK != 0
								if (enq_lock()) {
									dir_register(&strut->dj, [](void *data) {
										f_open_strut *strut = ((f_open_strut*) data);
										strut->mode |= FA_CREATE_ALWAYS; /* File is created */
										f_open_b(strut);
										return;
									},strut);
									return;
								} else {
									fResult = FR_TOO_MANY_OPEN_FILES;
								}
	#else
	#error
									fResult = dir_register(&dj);
	#endif
							}
							strut->mode |= FA_CREATE_ALWAYS; /* File is created */
						} else { /* Any object is already existing */
							if (strut->dj.obj.attr & (AM_READ_ONLY | AM_DIRECTORY)) { /* Cannot overwrite it (R/O or DIR) */
								fResult = FR_DENIED;
							} else {
								if (strut->mode & FA_CREATE_NEW)
									fResult = FR_EXIST; /* Cannot create as new file */
							}
						}
						f_open_b(strut);
						return;
					} else { /* Open an existing file */
						if (fResult == FR_OK) { /* Following succeeded */
							if (strut->dj.obj.attr & AM_DIRECTORY) { /* It is a directory */
								fResult = FR_NO_FILE;
							} else {
								if ((strut->mode & FA_WRITE) && (strut->dj.obj.attr & AM_READ_ONLY)) { /* R/O violation */
									fResult = FR_DENIED;
								}
							}
						}
					}
#endif
					f_open_a(strut);
					return;
				},strut); /* Follow the file path */
				return;
			}

			if (fResult != FR_OK) {
				strut->fp->obj.fs = 0; /* Invalidate file object on error */
			}
			delete strut;
			callNextCallback();
			return;
		}, s);
		return;
	}

	/*-----------------------------------------------------------------------*/
	/* Read File                                                             */
	/*-----------------------------------------------------------------------*/
	struct f_read_strut {
		FIL *fp; /* Pointer to the file object */
		void *buff; /* Pointer to data buffer */
		UINT btr; /* Number of bytes to read */
		UINT *br; /* Pointer to number of bytes read */
		FATFS *fs;
		DWORD clst, sect;
		FSIZE_t remain;
		UINT rcnt, cc, csect;
		BYTE *rbuff;
	}
	;

	void f_read_loop(f_read_strut *strut);
	void f_read_loop_cont(f_read_strut *strut) {
		strut->rbuff += strut->rcnt;
		strut->fp->fileReadWritePtr += strut->rcnt;
		*strut->br += strut->rcnt;
		strut->btr -= strut->rcnt;
		if (strut->btr) {
			f_read_loop(strut);
			return;
		} else {
			delete strut;
			callNextCallback();
			return;
		}
	}

	void f_read_c(f_read_strut *strut) {
		strut->rcnt = SS(
				strut->fs) - (UINT) strut->fp->fileReadWritePtr % SS(strut->fs); /* Number of bytes left in the sector */
		if (strut->rcnt > strut->btr)
			strut->rcnt = strut->btr; /* Clip it by btr if needed */
#if _NB_FS_TINY
#error
		if (move_window(fs, strut->fp->sectorNumber) != FR_OK)
		{
			ABORT(fs, FR_DISK_ERROR);
		} /* Move sector window */
		mem_cpy(strut->rbuff, strut->fs->win + strut->fp->fileReadWritePtr % SS(strut->fs), strut->rcnt); /* Extract partial sector */
#else
		mem_cpy(strut->rbuff, strut->fp->buf + strut->fp->fileReadWritePtr % SS(strut->fs), strut->rcnt); /* Extract partial sector */
#endif
		f_read_loop_cont(strut);
		return;
	}

	void f_read_d(f_read_strut *strut) {
		voidPtr = strut;
		disk_read(strut->fs->driveNumber, strut->fp->buf, strut->sect, 1, [](DRESULT dRes) {
			f_read_strut *strut = (f_read_strut*) voidPtr;
			if (dRes != RES_OK) {
				// ABORT(fs, FR_DISK_ERROR);
				strut->fp->err = FR_DISK_ERROR;
				fResult = FR_DISK_ERROR;
				delete strut;
				callNextCallback();
				return;
			} /* Fill sector cache */
			strut->fp->sectorNumber = strut->sect;
			f_read_c(strut);
			return;
		});
		return;
	}

	void f_read_b(f_read_strut *strut) {
		strut->sect = clust2sect(strut->fs, strut->fp->currentFilePtrCluster); /* Get current sector */
		if (!strut->sect) {
			strut->fp->err = FR_INT_ERROR;
			fResult = FR_INT_ERROR;
			delete strut;
			callNextCallback();
			return;
		}
		strut->sect += strut->csect;
		strut->cc = strut->btr / SS(strut->fs); /* When remaining bytes >= sector size, */
		if (strut->cc) { /* Read maximum contiguous sectors directly */
			if (strut->csect + strut->cc > strut->fs->clusterSize) { /* Clip at cluster boundary */
				strut->cc = strut->fs->clusterSize - strut->csect;
			}
			voidPtr = strut;
			disk_read(strut->fs->driveNumber, strut->rbuff, strut->sect, strut->cc, [](DRESULT dRes) {
				f_read_strut *strut = (f_read_strut*) voidPtr;
				if (dRes != RES_OK) {
					strut->fp->err = FR_DISK_ERROR;
					fResult = FR_DISK_ERROR;
					delete strut;
					callNextCallback();
					return;
				}
#if !_NB_FS_READONLY && _NB_FS_MINIMIZE <= 2 /* Replace one of the read sectors with cached data if it contains a dirty sector */
#if _NB_FS_TINY
#error
						  if (strut->fs->wflag && strut->fs->winSector - strut->sect < cc)
						  {
							  mem_cpy(rbuff + ((strut->fs->winSector - strut->sect) * SS(strut->fs)), strut->fs->win, SS(strut->fs));
						  }
#else
				if ((strut->fp->flag & FA_DIRTY) && strut->fp->sectorNumber - strut->sect < strut->cc) {
					mem_cpy(strut->rbuff + ((strut->fp->sectorNumber - strut->sect) * SS(strut->fs)), strut->fp->buf, SS(strut->fs));
				}
#endif
#endif
				strut->rcnt = SS(strut->fs) * strut->cc; /* Number of bytes transferred */
				f_read_loop_cont(strut);
				return;
			});
			return;
		}
#if !_NB_FS_TINY
		if (strut->fp->sectorNumber != strut->sect) { /* Load data sector if not in cache */
#if !_NB_FS_READONLY
			if (strut->fp->flag & FA_DIRTY) { /* Write-back dirty sector cache */
				voidPtr = strut;
				disk_write(strut->fs->driveNumber, strut->fp->buf, strut->fp->sectorNumber, 1, [](DRESULT dRes) {
					f_read_strut *strut = (f_read_strut*) voidPtr;
					if (dRes != RES_OK) {
						strut->fp->err = FR_DISK_ERROR;
						fResult = FR_DISK_ERROR;
						delete strut;
						callNextCallback();
						return;
					}
					strut->fp->flag &= (BYTE) ~FA_DIRTY;
					f_read_d(strut);
					return;
				});
				return;
			}
#endif
			f_read_d(strut);
			return;
		}
#endif
		strut->fp->sectorNumber = strut->sect;
		f_read_c(strut);
		return;
	}

	void f_read_a(f_read_strut *strut) {
		if (strut->clst < 2) {
			strut->fp->err = FR_INT_ERROR;
			fResult = FR_INT_ERROR;
			delete strut;
			callNextCallback();
			return;
		}
		if (strut->clst == 0xFFFFFFFF) {
			strut->fp->err = FR_DISK_ERROR;
			fResult = FR_DISK_ERROR;
			delete strut;
			callNextCallback();
			return;
		}
		strut->fp->currentFilePtrCluster = strut->clst; /* Update current cluster */
		f_read_b(strut);
		return;
	}

	void f_read_loop(f_read_strut *strut) {
		if (strut->btr) /* Repeat until all data read */
		{
			if (strut->fp->fileReadWritePtr % SS(strut->fs) == 0) { /* On the sector boundary? */
				strut->csect = (UINT) (strut->fp->fileReadWritePtr / SS(strut->fs) & (strut->fs->clusterSize - 1)); /* Sector offset in the cluster */
				if (strut->csect == 0) { /* On the cluster boundary? */
					if (strut->fp->fileReadWritePtr == 0) { /* On the top of the file? */
						strut->clst = strut->fp->obj.startCluster; /* Follow cluster chain from the origin */
					} else { /* Middle or end of the file */
#if _NB_USE_FASTSEEK
						if (strut->fp->clusterLinkMapTablePtr) {
							strut->clst = clmt_clust(strut->fp, strut->fp->fileReadWritePtr); /* Get cluster# from the CLMT */
						} else
#endif
						{
							get_fat(&strut->fp->obj, strut->fp->currentFilePtrCluster, [](void *data) {
								f_read_strut *strut = ((f_read_strut*) data);
								strut->clst = getFatValue;
								f_read_a(strut);
								return;
							}, strut); /* Follow cluster chain on the FAT */
							return;
						}
					}
					f_read_a(strut);
					return;
				}
				f_read_b(strut);
				return;
			}
			f_read_c(strut);
			return;
		}
		fResult = FR_OK;
		delete strut;
		callNextCallback();
		return;
	}

	void f_read(FIL *fpInput, /* Pointer to the file object */
	void *buffInput, /* Pointer to data buffer */
	UINT btrInput, /* Number of bytes to read */
	UINT *brInput /* Pointer to number of bytes read */
	, void (*callback)(void*)) {
		addCallback(callback, &fResult);
		f_read_strut *s = new f_read_strut();
		s->fp = fpInput;
		s->buff = buffInput;
		s->btr = btrInput;
		s->br = brInput;
		s->rbuff = (BYTE*) buffInput;

		*s->br = 0; /* Clear read byte counter */
		validate(&s->fp->obj, &s->fs, [](void *data) {
			f_read_strut *strut = ((f_read_strut*) data);
			if (fResult != FR_OK || (fResult = (FRESULT) strut->fp->err) != FR_OK) {
				delete strut;
				callNextCallback();
				return;
			} /* Check validity */
			if (!(strut->fp->flag & FA_READ)) {
				fResult = FR_DENIED;
				delete strut;
				callNextCallback();
				return;
			} /* Check access mode */
			strut->remain = strut->fp->obj.objSize - strut->fp->fileReadWritePtr;
			if (strut->btr > strut->remain) {
				strut->btr = (UINT) strut->remain;
			} /* Truncate btr by remaining bytes */

			f_read_loop(strut);
			return;
		}, s);
		return; /* Check validity of the file object */
	}

	void f_read(FIL *fpInput, /* Pointer to the file object */
	void *buffInput, /* Pointer to data buffer */
	UINT btrInput, /* Number of bytes to read */
	UINT *brInput /* Pointer to number of bytes read */
	, void (*callback)(void*), void *data) {
		addCallback(callback, data);
		f_read_strut *s = new f_read_strut();
		s->fp = fpInput;
		s->buff = buffInput;
		s->btr = btrInput;
		s->br = brInput;
		s->rbuff = (BYTE*) buffInput;

		*s->br = 0; /* Clear read byte counter */
		validate(&s->fp->obj, &s->fs, [](void *data) {
			f_read_strut *strut = ((f_read_strut*) data);
			if (fResult != FR_OK || (fResult = (FRESULT) strut->fp->err) != FR_OK) {
				delete strut;
				callNextCallback();
				return;
			} /* Check validity */
			if (!(strut->fp->flag & FA_READ)) {
				fResult = FR_DENIED;
				delete strut;
				callNextCallback();
				return;
			} /* Check access mode */
			strut->remain = strut->fp->obj.objSize - strut->fp->fileReadWritePtr;
			if (strut->btr > strut->remain) {
				strut->btr = (UINT) strut->remain;
			} /* Truncate btr by remaining bytes */

			f_read_loop(strut);
			return;
		}, s);
		return; /* Check validity of the file object */
	}

#if !_NB_FS_READONLY
	/*-----------------------------------------------------------------------*/
	/* Write File                                                            */
	/*-----------------------------------------------------------------------*/
	struct f_write_strut {
		FIL *fp; /* Pointer to the file object */
		const void *buff; /* Pointer to the data to be written */
		UINT btw; /* Number of bytes to write */
		UINT *bw; /* Pointer to number of bytes written */
		FATFS *fs;
		DWORD clst, sect;
		UINT wcnt, cc, csect;
		const BYTE *wbuff;
	}
	;

	void f_write_end(f_write_strut *strut) {
		strut->fp->flag |= FA_MODIFIED; /* Set file change flag */

		fResult = FR_OK;
		delete strut;
		callNextCallback();
		return;
	}

	void f_write_loop(f_write_strut *strut);
	void f_write_loop_cont(f_write_strut *strut) {
		strut->wbuff += strut->wcnt;
		strut->fp->fileReadWritePtr += strut->wcnt;
		strut->fp->obj.objSize = (strut->fp->fileReadWritePtr > strut->fp->obj.objSize) ? strut->fp->fileReadWritePtr : strut->fp->obj.objSize;
		*strut->bw += strut->wcnt;
		strut->btw -= strut->wcnt;

		if (strut->btw) {
			f_write_loop(strut);
			return;
		} else {
			f_write_end(strut);
			return;
		}
	}

	void f_write_a(f_write_strut *strut) {
		strut->wcnt = SS(
				strut->fs) - (UINT) strut->fp->fileReadWritePtr % SS(strut->fs); /* Number of bytes left in the sector */
		if (strut->wcnt > strut->btw) {
			strut->wcnt = strut->btw;
		} /* Clip it by btw if needed */
#if _NB_FS_TINY
#error
		if (move_window(strut->fs, strut->fp->sectorNumber) != FR_OK)
		{
			ABORT(strut->fs, FR_DISK_ERROR);
		} /* Move sector window */
		mem_cpy(strut->fs->win + strut->fp->fileReadWritePtr % SS(strut->fs), strut->wbuff, strut->wcnt); /* Fit data to the sector */
		strut->fs->wflag = 1;
#else
		mem_cpy(strut->fp->buf + strut->fp->fileReadWritePtr % SS(strut->fs), strut->wbuff, strut->wcnt); /* Fit data to the sector */
		strut->fp->flag |= FA_DIRTY;
#endif
		f_write_loop_cont(strut);
		return;
	}

	void f_write_b_a(f_write_strut *strut) {
		strut->sect = clust2sect(strut->fs, strut->fp->currentFilePtrCluster); /* Get current sector */
		if (!strut->sect) {
			strut->fp->err = FR_INT_ERROR;
			fResult = FR_INT_ERROR;
			delete strut;
			callNextCallback();
			return;
		}
		strut->sect += strut->csect;
		strut->cc = strut->btw / SS(strut->fs); /* When remaining bytes >= sector size, */
		if (strut->cc) { /* Write maximum contiguous sectors directly */
			if (strut->csect + strut->cc > strut->fs->clusterSize) { /* Clip at cluster boundary */
				strut->cc = strut->fs->clusterSize - strut->csect;
			}
			voidPtr = strut;
			disk_write(strut->fs->driveNumber, strut->wbuff, strut->sect, strut->cc, [](DRESULT dRes) {
				f_write_strut *strut = (f_write_strut*) voidPtr;
				if (dRes != RES_OK) {
					strut->fp->err = FR_DISK_ERROR;
					fResult = FR_DISK_ERROR;
					delete strut;
					callNextCallback();
					return;
				}
#if _NB_FS_MINIMIZE <= 2
#if _NB_FS_TINY
#error
						   if (strut->fs->winSector - sectorNumber < cc)
						   { /* Refill sector cache if it gets invalidated by the direct write */
							   mem_cpy(strut->fs->win, wbuff + ((strut->fs->winSector - sectorNumber) * SS(strut->fs)), SS(strut->fs));
							   strut->fs->wflag = 0;
						   }
#else
				if (strut->fp->sectorNumber - strut->sect < strut->cc) { /* Refill sector cache if it gets invalidated by the direct write */
					mem_cpy(strut->fp->buf, strut->wbuff + ((strut->fp->sectorNumber - strut->sect) * SS(strut->fs)), SS(strut->fs));
					strut->fp->flag &= (BYTE) ~FA_DIRTY;
				}
#endif
#endif
				strut->wcnt = SS(strut->fs) * strut->cc; /* Number of bytes transferred */
				f_write_loop_cont(strut);
				return;
			});
			return;
		}
#if _NB_FS_TINY
#error
		if (fp->fileReadWritePtr >= fp->obj.objSize)
		{ /* Avoid silly cache filling on the growing edge */
			if (sync_window(strut->fs) != FR_OK)
			{
				ABORT(fs, FR_DISK_ERROR);
			}
			strut->fs->winSector = strut->sect;
		}
#else
		voidPtr = strut;
		disk_read(strut->fs->driveNumber, strut->fp->buf, strut->sect, 1, [](DRESULT dRes) {
			f_write_strut *strut = (f_write_strut*) voidPtr;
			if (strut->fp->sectorNumber != strut->sect && /* Fill sector cache with file data */
			strut->fp->fileReadWritePtr < strut->fp->obj.objSize && dRes != RES_OK) {
				strut->fp->err = FR_DISK_ERROR;
				fResult = FR_DISK_ERROR;
				delete strut;
				callNextCallback();
				return;
			}
#endif
			strut->fp->sectorNumber = strut->sect;
			f_write_a(strut);
			return;
		});
		return;
	}

	void f_write_b(f_write_strut *strut) {
#if _NB_FS_TINY
#error
	if (strut->fs->winSector == fp->sectorNumber && sync_window(strut->fs) != FR_OK)
	{
		ABORT(fs, FR_DISK_ERROR);
	} /* Write-back sector cache */
#else
		if (strut->fp->flag & FA_DIRTY) { /* Write-back sector cache */
			voidPtr = strut;
			disk_write(strut->fs->driveNumber, strut->fp->buf, strut->fp->sectorNumber, 1, [](DRESULT dRes) {
				f_write_strut *strut = (f_write_strut*) voidPtr;
				if (dRes != RES_OK) {
					strut->fp->err = FR_DISK_ERROR;
					fResult = FR_DISK_ERROR;
					delete strut;
					callNextCallback();
					return;
				}
				strut->fp->flag &= (BYTE) ~FA_DIRTY;
				f_write_b_a(strut);
				return;
			});
			return;
		}
#endif

		f_write_b_a(strut);
	}

	void f_write_a_a(f_write_strut *strut) {
		if (strut->clst == 0) {
			f_write_end(strut);
			return;
		} /* Could not allocate a new cluster (disk full) */
		if (strut->clst == 1) {
			strut->fp->err = FR_INT_ERROR;
			fResult = FR_INT_ERROR;
			delete strut;
			callNextCallback();
			return;
		}
		if (strut->clst == 0xFFFFFFFF) {
			strut->fp->err = FR_DISK_ERROR;
			fResult = FR_DISK_ERROR;
			delete strut;
			callNextCallback();
			return;
		}
		strut->fp->currentFilePtrCluster = strut->clst; /* Update current cluster */
		if (strut->fp->obj.startCluster == 0) {
			strut->fp->obj.startCluster = strut->clst;
		} /* Set start cluster if the first write */
		f_write_b(strut);
		return;
	}

	void f_write_loop(f_write_strut *strut) {
		if (strut->fp->fileReadWritePtr % SS(strut->fs) == 0) { /* On the sector boundary? */
			strut->csect = (UINT) (strut->fp->fileReadWritePtr / SS(strut->fs)) & (strut->fs->clusterSize - 1); /* Sector offset in the cluster */
			if (strut->csect == 0) { /* On the cluster boundary? */
				if (strut->fp->fileReadWritePtr == 0) { /* On the top of the file? */
					strut->clst = strut->fp->obj.startCluster; /* Follow from the origin */
					if (strut->clst == 0) { /* If no cluster is allocated, */
						create_chain(&strut->fp->obj, 0, [](void *data) {
							f_write_strut *strut = ((f_write_strut*) data);
							strut->clst = createChainValue; /* create a new cluster chain */
							f_write_a_a(strut);
							return;
						}, strut);
						return;
					}
				} else { /* On the middle or end of the file */
#if _NB_USE_FASTSEEK
					if (strut->fp->clusterLinkMapTablePtr) {
						strut->clst = clmt_clust(strut->fp, strut->fp->fileReadWritePtr); /* Get cluster# from the CLMT */
					} else
#endif
					{
						create_chain(&strut->fp->obj, strut->fp->currentFilePtrCluster, [](void *data) {
							f_write_strut *strut = ((f_write_strut*) data);
							strut->clst = createChainValue; /* Follow or stretch cluster chain on the FAT */
							f_write_a_a(strut);
							return;
						}, strut);
						return;
					}
				}
				f_write_a_a(strut);
				return;
			}
			f_write_b(strut);
			return;
		}

		f_write_a(strut);
		return;
	}

	void f_write(FIL *fp, /* Pointer to the file object */
	const void *buff, /* Pointer to the data to be written */
	UINT btw, /* Number of bytes to write */
	UINT *bw /* Pointer to number of bytes written */
	, void (*callback)(void*)) {
		addCallback(callback, &fResult);
		f_write_strut *s = new f_write_strut();
		s->fp = fp;
		s->buff = buff;
		s->btw = btw;
		s->bw = bw;
		s->wbuff = (const BYTE*) buff;

		*s->bw = 0; /* Clear write byte counter */
		validate(&s->fp->obj, &s->fs, [](void *data) { /* Check validity of the file object */
			f_write_strut *strut = ((f_write_strut*) data);
			if (fResult != FR_OK || (fResult = (FRESULT) strut->fp->err) != FR_OK) {
				delete strut;
				callNextCallback();
				return;
			} /* Check validity */
			if (!(strut->fp->flag & FA_WRITE)) {
				fResult = FR_DENIED;
				delete strut;
				callNextCallback();
				return;
			} /* Check access mode */

			/* Check fileReadWritePtr wrap-around (file size cannot reach 4GiB on FATxx) */
			if ((!_NB_FS_EXFAT || strut->fs->fs_type != FS_EXFAT) && (DWORD) (strut->fp->fileReadWritePtr + strut->btw) < (DWORD) strut->fp->fileReadWritePtr)
		{
			strut->btw = (UINT)(0xFFFFFFFF - (DWORD)strut->fp->fileReadWritePtr);
		}
			if (strut->btw) {
				f_write_loop(strut);
				return;
			} else {
				f_write_end(strut);
				return;
			}
		}, s);
		return;
	}

	void f_write(FIL *fp, /* Pointer to the file object */
	const void *buff, /* Pointer to the data to be written */
	UINT btw, /* Number of bytes to write */
	UINT *bw /* Pointer to number of bytes written */
	, void (*callback)(void*), void *data) {
		addCallback(callback, data);
		f_write_strut *s = new f_write_strut();
		s->fp = fp;
		s->buff = buff;
		s->btw = btw;
		s->bw = bw;
		s->wbuff = (const BYTE*) buff;

		*s->bw = 0; /* Clear write byte counter */
		validate(&s->fp->obj, &s->fs, [](void *data) {
			f_write_strut *strut = ((f_write_strut*) data);
			if (fResult != FR_OK || (fResult = (FRESULT) strut->fp->err) != FR_OK) {
				delete strut;
				callNextCallback();
				return;
			} /* Check validity */
			if (!(strut->fp->flag & FA_WRITE)) {
				fResult = FR_DENIED;
				delete strut;
				callNextCallback();
				return;
			} /* Check access mode */

			/* Check fileReadWritePtr wrap-around (file size cannot reach 4GiB on FATxx) */
			if ((!_NB_FS_EXFAT || strut->fs->fs_type != FS_EXFAT) && (DWORD) (strut->fp->fileReadWritePtr + strut->btw) < (DWORD) strut->fp->fileReadWritePtr)
		{
			strut->btw = (UINT)(0xFFFFFFFF - (DWORD)strut->fp->fileReadWritePtr);
		}
			if (strut->btw) {
				f_write_loop(strut);
				return;
			} else {
				f_write_end(strut);
				return;
			}
		}, s);
		return; /* Check validity of the file object */
	}

	/*-----------------------------------------------------------------------*/
	/* Synchronize the File                                                  */
	/*-----------------------------------------------------------------------*/
	struct f_sync_strut {
		FIL *fp; /* Pointer to the file object */
		FATFS *fs;
		DWORD tm;
		BYTE *dir;
#if _NB_FS_EXFAT
		DIR dj;
#endif
	}
	;

	void f_sync_a(f_sync_strut *strut) {
#if _NB_FS_EXFAT
		if (strut->fs->fs_type == FS_EXFAT) {
			fill_first_frag(&strut->fp->obj, [](void *data) {
				f_sync_strut *strut = ((f_sync_strut*) data);
				if (fResult == FR_OK) {
					fill_last_frag(&strut->fp->obj, strut->fp->currentFilePtrCluster, 0xFFFFFFFF, [](void *data) {
						f_sync_strut *strut = ((f_sync_strut*) data);
						if (fResult == FR_OK) {
							INIT_NAMBUF(strut->fs);
							load_obj_dir(&strut->dj, &strut->fp->obj, [](void *data) {
								f_sync_strut *strut = ((f_sync_strut*) data);
								if (fResult == FR_OK) {
									strut->fs->dirBuffer[XDIR_Attr] |= AM_ARCHIVE; /* Set archive bit */
									strut->fs->dirBuffer[XDIR_GenFlags] = strut->fp->obj.stat | 1; /* Update file allocation info */
									st_dword(strut->fs->dirBuffer + XDIR_FstClus, strut->fp->obj.startCluster);
									st_qword(strut->fs->dirBuffer + XDIR_FileSize, strut->fp->obj.objSize);
									st_qword(strut->fs->dirBuffer + XDIR_ValidFileSize, strut->fp->obj.objSize);
									st_dword(strut->fs->dirBuffer + XDIR_ModTime, strut->tm); /* Update modified time */
									strut->fs->dirBuffer[XDIR_ModTime10] = 0;
									st_dword(strut->fs->dirBuffer + XDIR_AccTime, 0);
									store_xdir(&strut->dj, [](void *data) {
										f_sync_strut *strut = ((f_sync_strut*) data);
										if (fResult == FR_OK) {
											sync_fs(strut->fs, [](void *data) {
												f_sync_strut *strut = ((f_sync_strut*) data);
												strut->fp->flag &= (BYTE) ~FA_MODIFIED;
												FREE_NAMBUF();
												delete strut;
												callNextCallback();
												return;
											},strut);
											return;
										} FREE_NAMBUF();
										delete strut;
										callNextCallback();
										return;
									},strut); /* Restore it to the directory */
									return;
								} FREE_NAMBUF();
								delete strut;
								callNextCallback();
								return;
							},strut); /* Load directory entry block */
							return;
						}
						delete strut;
						callNextCallback();
						return;
						return;
					},strut); /* Fill last fragment on the FAT if needed */
					return;
				}
				delete strut;
				callNextCallback();
				return;
			}, strut); /* Fill first fragment on the FAT if needed */
			return;
		} else
#endif
		{
			move_window(strut->fs, strut->fp->dirSectNumber, [](void *data) {
				f_sync_strut *strut = ((f_sync_strut*) data);
				if (fResult == FR_OK) {
					strut->dir = strut->fp->dirEntryPtr;
					strut->dir[DIR_Attr] |= AM_ARCHIVE; /* Set archive bit */
					st_clust(strut->fp->obj.fs, strut->dir, strut->fp->obj.startCluster); /* Update file allocation info  */
					st_dword(strut->dir + DIR_FileSize, (DWORD) strut->fp->obj.objSize); /* Update file size */
					st_dword(strut->dir + DIR_ModTime, strut->tm); /* Update modified time */
					st_word(strut->dir + DIR_LstAccDate, 0);
					strut->fs->wflag = 1;
					sync_fs(strut->fs, [](void *data) {
						f_sync_strut *strut = ((f_sync_strut*) data);
						strut->fp->flag &= (BYTE) ~FA_MODIFIED;
						delete strut;
						callNextCallback();
						return;
					},strut); /* Restore it to the directory */
					return;
				}
				delete strut;
				callNextCallback();
				return;
			}, strut);
			return;
			delete strut;
			callNextCallback();
			return;
		}
	}

	void f_sync(FIL *fp /* Pointer to the file object */
	, void (*callback)(void*)) {
		addCallback(callback, &fResult);
		f_sync_strut *s = new f_sync_strut();
		s->fp = fp;
#if _NB_FS_EXFAT
		DEF_NAMBUF
#endif

		validate(&s->fp->obj, &s->fs, [](void *data) {
			f_sync_strut *strut = ((f_sync_strut*) data);

			if (fResult == FR_OK) {
				if (strut->fp->flag & FA_MODIFIED) { /* Is there any change to the file? */
#if !_NB_FS_TINY
			if (strut->fp->flag & FA_DIRTY) { /* Write-back cached data if needed */
				voidPtr = strut;
				disk_write(strut->fs->driveNumber, strut->fp->buf, strut->fp->sectorNumber, 1, [](DRESULT dRes) {
					f_sync_strut *strut = (f_sync_strut*) voidPtr;
					if (dRes != RES_OK) {
						fResult = FR_DISK_ERROR;
						delete strut;
						callNextCallback();
						return;
					}
					strut->fp->flag &= (BYTE) ~FA_DIRTY;
					strut->tm = GET_FATTIME(); /* Modified time */
					f_sync_a(strut);
					return;
				});
				return;
			}
#endif
			/* Update the directory entry */
			strut->tm = GET_FATTIME(); /* Modified time */
			f_sync_a(strut);
			return;
		}
	}
	delete strut;
	callNextCallback();
	return;
}		, s);
		return; /* Check validity of the file object */
	}

	void f_sync(FIL *fp, /* Pointer to the file object */
	void (*callback)(void*), void *data) {
		addCallback(callback, data);
		f_sync_strut *s = new f_sync_strut();
		s->fp = fp;
#if _NB_FS_EXFAT
		DEF_NAMBUF
#endif

		validate(&s->fp->obj, &s->fs, [](void *data) {
			f_sync_strut *strut = ((f_sync_strut*) data);

			if (fResult == FR_OK) {
				if (strut->fp->flag & FA_MODIFIED) { /* Is there any change to the file? */
#if !_NB_FS_TINY
			if (strut->fp->flag & FA_DIRTY) { /* Write-back cached data if needed */
				voidPtr = strut;
				disk_write(strut->fs->driveNumber, strut->fp->buf, strut->fp->sectorNumber, 1, [](DRESULT dRes) {
					f_sync_strut *strut = (f_sync_strut*) voidPtr;
					if (dRes != RES_OK) {
						fResult = FR_DISK_ERROR;
						delete strut;
						callNextCallback();
						return;
					}
					strut->fp->flag &= (BYTE) ~FA_DIRTY;
					strut->tm = GET_FATTIME(); /* Modified time */
					f_sync_a(strut);
					return;
				});
				return;
			}
#endif
			/* Update the directory entry */
			strut->tm = GET_FATTIME(); /* Modified time */
			f_sync_a(strut);
			return;
		}
	}
	delete strut;
	callNextCallback();
	return;
}		, s);
		return; /* Check validity of the file object */
	}

#endif /* !_FS_READONLY */

	/*-----------------------------------------------------------------------*/
	/* Close File                                                            */
	/*-----------------------------------------------------------------------*/
	struct f_close_strut {
		FIL *fp; /* Pointer to the file object to be closed */
		FATFS *fs;
	}
	;

	void f_close(FIL *fp /* Pointer to the file object to be closed */
	, void (*callback)(void*)) {
		addCallback(callback, &fResult);
		f_close_strut *s = new f_close_strut();
		s->fp = fp;

#if !_NB_FS_READONLY
		f_sync(s->fp, [](void *data) {
			f_close_strut *strut = ((f_close_strut*) data);
			if (fResult == FR_OK)
#endif
					{
				validate(&strut->fp->obj, &strut->fs, [](void *data) {
					f_close_strut *strut = ((f_close_strut*) data);
					if (fResult == FR_OK) {
#if _NB_FS_LOCK != 0
					fResult = dec_lock(strut->fp->obj.lockid); /* Decrement file open counter */
					if (fResult == FR_OK)
#endif
							{
						strut->fp->obj.fs = 0; /* Invalidate file object */
					}
#if _NB_FS_REENTRANT
									unlock_fs(fs, FR_OK); /* Unlock volume */
#endif
				}
				delete strut;
				callNextCallback();
				return;
			}	, strut); /* Lock volume */
				return;
			}
			delete strut;
			callNextCallback();
			return;
		}, s);
		return; /* Flush cached data */
	}

#if _NB_FS_RPATH >= 1
/*-----------------------------------------------------------------------*/
/* Change Current Directory or Current Drive, Get Current Directory      */
/*-----------------------------------------------------------------------*/

#if _NB_VOLUMES >= 2
#error
FRESULT f_chdrive(
	const TCHAR *path /* Drive number */
	,
	void (*callback)(void *))
{
	addCallback(callback, data);
	int vol;

	/* Get logical drive number */
	vol = get_ldnumber(&path);
	if (vol < 0)
		return FR_INVALID_DRIVE;

	CurrVol = (BYTE)vol; /* Set it as current volume */

	return FR_OK;
}
#endif

#error
FRESULT f_chdir(
	const TCHAR *path /* Pointer to the directory path */
	,
	void (*callback)())
{
	addCallback(callback, data);
	FRESULT res;
	DIR dj;
	FATFS *fs;
	DEF_NAMBUF

	/* Get logical drive */
	res = find_volume(&path, &fs, 0);
	if (res == FR_OK)
	{
		dj.obj.fs = fs;
		INIT_NAMBUF(fs);
		res = follow_path(&dj, path); /* Follow the path */
		if (res == FR_OK)
		{ /* Follow completed */
			if (dj.fn[NSFLAG] & NS_NONAME)
			{
				fs->cdir = dj.obj.startCluster; /* It is the start directory itself */
#if _NB_FS_EXFAT
				if (fs->fs_type == FS_EXFAT)
				{
					fs->cdc_scl = dj.obj.containingDirectoryStartCluster;
					fs->cdc_size = dj.obj.sizeOfContainingDirectory;
					fs->cdc_ofs = dj.obj.offsetInContainingDirectory;
				}
#endif
			}
			else
			{
				if (dj.obj.attr & AM_DIRECTORY)
				{ /* It is a sub-directory */
#if _NB_FS_EXFAT
					if (fs->fs_type == FS_EXFAT)
					{
						fs->cdir = ld_dword(fs->dirBuffer + XDIR_FstClus); /* Sub-directory cluster */
						fs->cdc_scl = dj.obj.startCluster;				   /* Save containing directory information */
						fs->cdc_size = ((DWORD)dj.obj.objSize & 0xFFFFFF00) | dj.obj.stat;
						fs->cdc_ofs = dj.currentEntryBlockOffset;
					}
					else
#endif
					{
						fs->cdir = ld_clust(fs, dj.dirPtr); /* Sub-directory cluster */
					}
				}
				else
				{
					res = FR_NO_PATH; /* Reached but a file */
				}
			}
		}
		FREE_NAMBUF();
		if (res == FR_NO_FILE)
			res = FR_NO_PATH;
	}

	LEAVE_FF(fs, res);
}

#if _NB_FS_RPATH >= 2
#error
FRESULT f_getcwd(
	TCHAR *buff, /* Pointer to the directory path */
	UINT len	 /* Size of path */
	,
	void (*callback)(void *))
{
	addCallback(callback, data);
	FRESULT res;
	DIR dj;
	FATFS *fs;
	UINT i, n;
	DWORD ccl;
	TCHAR *tp;
	FILINFO fno;
	DEF_NAMBUF

	*buff = 0;
	/* Get logical drive */
	res = find_volume((const TCHAR **)&buff, &fs, 0); /* Get current volume */
	if (res == FR_OK)
	{
		dj.obj.fs = fs;
		INIT_NAMBUF(fs);
		i = len; /* Bottom of buffer (directory stack base) */
		if (!_NB_FS_EXFAT || fs->fs_type != FS_EXFAT)
		{									/* (Cannot do getcwd on exFAT and returns root path) */
			dj.obj.startCluster = fs->cdir; /* Start to follow upper directory from current directory */
			while ((ccl = dj.obj.startCluster) != 0)
			{									/* Repeat while current directory is a sub-directory */
				res = dir_sdi(&dj, 1 * SZDIRE); /* Get parent directory */
				if (res != FR_OK)
					break;
				res = move_window(fs, dj.sectorNumber);
				if (res != FR_OK)
					break;
				dj.obj.startCluster = ld_clust(fs, dj.dirPtr); /* Goto parent directory */
				res = dir_sdi(&dj, 0);
				if (res != FR_OK)
					break;
				do
				{ /* Find the entry links to the child directory */
					res = dir_read(&dj, 0);
					if (res != FR_OK)
						break;
					if (ccl == ld_clust(fs, dj.dirPtr))
						break; /* Found the entry */
					res = dir_next(&dj, 0);
				} while (res == FR_OK);
				if (res == FR_NO_FILE)
					res = FR_INT_ERROR; /* It cannot be 'not found'. */
				if (res != FR_OK)
					break;
				get_fileinfo(&dj, &fno); /* Get the directory name and push it to the buffer */
				for (n = 0; fno.fname[n]; n++)
					;
				if (i < n + 3)
				{
					res = FR_NOT_ENOUGH_CORE;
					break;
				}
				while (n)
					buff[--i] = fno.fname[--n];
				buff[--i] = '/';
			}
		}
		tp = buff;
		if (res == FR_OK)
		{
#if _NB_VOLUMES >= 2
			*tp++ = '0' + CurrVol; /* Put drive number */
			*tp++ = ':';
#endif
			if (i == len)
			{ /* Root-directory */
				*tp++ = '/';
			}
			else
			{	   /* Sub-directroy */
				do /* Add stacked path str */
					*tp++ = buff[i++];
				while (i < len);
			}
		}
		*tp = 0;
		FREE_NAMBUF();
	}

	LEAVE_FF(fs, res);
}

#endif /* _NB_FS_RPATH >= 2 */
#endif /* _NB_FS_RPATH >= 1 */

#if _NB_FS_MINIMIZE <= 2
	/*-----------------------------------------------------------------------*/
	/* Seek File R/W Pointer                                                 */
	/*-----------------------------------------------------------------------*/
	struct f_lseek_strut {
		FIL *fp; /* Pointer to the file object */
		FSIZE_t ofs; /* File pointer from top of file */
		FATFS *fs;
		DWORD clst, bcs, nsect;
		FSIZE_t ifptr;
#if _NB_USE_FASTSEEK
		DWORD cl, pcl, ncl, tcl, dsc, tlen, ulen, *tbl;
#endif
	}
	;

	void f_lseek_loop_end(f_lseek_strut *strut) {
		*strut->fp->clusterLinkMapTablePtr = strut->ulen; /* Number of items used */
		if (strut->ulen <= strut->tlen) {
			*strut->tbl = 0; /* Terminate table */
		} else {
			fResult = FR_NOT_ENOUGH_CORE; /* Given table size is smaller than required */
		}
		delete strut;
		callNextCallback();
		return;
	}

	void f_lseek_loop(f_lseek_strut *strut);
	void f_lseek_loop_a(f_lseek_strut *strut) {
		strut->pcl = strut->cl;
		strut->ncl++;
		get_fat(&strut->fp->obj, strut->cl, [](void *data) {
			f_lseek_strut *strut = ((f_lseek_strut*) data);

			strut->cl = getFatValue;
			if (strut->cl <= 1) {
				strut->fp->err = FR_INT_ERROR;
				fResult = FR_INT_ERROR;
				delete strut;
				callNextCallback();
				return;
			}
			if (strut->cl == 0xFFFFFFFF) {
				strut->fp->err = FR_DISK_ERROR;
				fResult = FR_DISK_ERROR;
				delete strut;
				callNextCallback();
				return;
			}

			if (strut->cl == strut->pcl + 1) {
				f_lseek_loop_a(strut);
				return;
			} else {
				if (strut->ulen <= strut->tlen) { /* Store the length and top of the fragment */
					*strut->tbl++ = strut->ncl;
					*strut->tbl++ = strut->tcl;
				}
				if (strut->cl < strut->fs->numberOfFatEntries) {
					f_lseek_loop(strut);
					return;
				} /* Repeat until end of chain */
				else {
					f_lseek_loop_end(strut);
					return;
				}
			}
		}, strut);
		return;
	}

	void f_lseek_loop(f_lseek_strut *strut) {
		/* Get a fragment */
		strut->tcl = strut->cl;
		strut->ncl = 0;
		strut->ulen += 2; /* Top, length and used items */

		f_lseek_loop_a(strut);
		return;
	}

	void f_lseek_d(f_lseek_strut *strut) {

#if !_NB_FS_TINY
		voidPtr = strut;
		disk_read(strut->fs->driveNumber, strut->fp->buf, strut->nsect, 1, [](DRESULT dRes) {
			f_lseek_strut *strut = (f_lseek_strut*) voidPtr;
			if (dRes != RES_OK) {
				strut->fp->err = FR_DISK_ERROR;
				fResult = FR_DISK_ERROR;
				delete strut;
				callNextCallback();
				return;
			} /* Fill sector cache */

			strut->fp->sectorNumber = strut->nsect;
			delete strut;
			callNextCallback();
			return;
		});
		return;
#else
#error
	strut->fp->sectorNumber = strut->nsect;
	delete strut;
	callNextCallback();
	return;
#endif
	}

	void f_lseek_c(f_lseek_strut *strut) {
		if (!_NB_FS_READONLY && strut->fp->fileReadWritePtr > strut->fp->obj.objSize) { /* Set file change flag if the file size is extended */
			strut->fp->obj.objSize = strut->fp->fileReadWritePtr;
			strut->fp->flag |= FA_MODIFIED;
		}
		if (strut->fp->fileReadWritePtr % SS(strut->fs) && strut->nsect != strut->fp->sectorNumber) { /* Fill sector cache if needed */
#if !_NB_FS_TINY
#if !_NB_FS_READONLY
			if (strut->fp->flag & FA_DIRTY) { /* Write-back dirty sector cache */
				voidPtr = strut;
				disk_write(strut->fs->driveNumber, strut->fp->buf, strut->fp->sectorNumber, 1, [](DRESULT dRes) {
					f_lseek_strut *strut = (f_lseek_strut*) voidPtr;
					if (dRes != RES_OK) {
						strut->fp->err = FR_DISK_ERROR;
						fResult = FR_DISK_ERROR;
						delete strut;
						callNextCallback();
						return;
					}
					strut->fp->flag &= (BYTE) ~FA_DIRTY;
					f_lseek_d(strut);
					return;
				});
				return;
			}
#endif
#endif
			f_lseek_d(strut);
			return;
		}
		delete strut;
		callNextCallback();
		return;
	}

	void f_lseek_b_loop_end(f_lseek_strut *strut) {
		strut->fp->fileReadWritePtr += strut->ofs;
		if (strut->ofs % SS(strut->fs)) {
			strut->nsect = clust2sect(strut->fs, strut->clst); /* Current sector */
			if (!strut->nsect) {
				strut->fp->err = FR_INT_ERROR;
				fResult = FR_INT_ERROR;
				delete strut;
				callNextCallback();
				return;
			}
			strut->nsect += (DWORD) (strut->ofs / SS(strut->fs));
		}
		f_lseek_c(strut);
		return;
	}

	void f_lseek_b_loop(f_lseek_strut *strut);
	void f_lseek_b_loop_a(f_lseek_strut *strut) {
		if (strut->clst == 0xFFFFFFFF) {
			strut->fp->err = FR_DISK_ERROR;
			fResult = FR_DISK_ERROR;
			delete strut;
			callNextCallback();
			return;
		}
		if (strut->clst <= 1 || strut->clst >= strut->fs->numberOfFatEntries) {
			strut->fp->err = FR_INT_ERROR;
			fResult = FR_INT_ERROR;
			delete strut;
			callNextCallback();
			return;
		}
		strut->fp->currentFilePtrCluster = strut->clst;
		f_lseek_b_loop(strut);
		return;
	}

	void f_lseek_b_loop(f_lseek_strut *strut) { /* Cluster following loop */
		if (strut->ofs > strut->bcs) {
			strut->ofs -= strut->bcs;
			strut->fp->fileReadWritePtr += strut->bcs;
#if !_NB_FS_READONLY
			if (strut->fp->flag & FA_WRITE) { /* Check if in write mode or not */
				if (_NB_FS_EXFAT && strut->fp->fileReadWritePtr > strut->fp->obj.objSize) { /* No FAT chain object needs correct objSize to generate FAT value */
					strut->fp->obj.objSize = strut->fp->fileReadWritePtr;
					strut->fp->flag |= FA_MODIFIED;
				}
				create_chain(&strut->fp->obj, strut->clst, [](void *data) {
					f_lseek_strut *strut = ((f_lseek_strut*) data);	/* Follow chain with forceed stretch */
					strut->clst = createChainValue;
					if (strut->clst == 0) { /* Clip file size in case of disk full */
						strut->ofs = 0;
						f_lseek_b_loop_end(strut);
						return;
					}
					f_lseek_b_loop_a(strut);
					return;
				}, strut);
				return;
			} else
#endif
			{
				get_fat(&strut->fp->obj, strut->clst, [](void *data) {
					f_lseek_strut *strut = ((f_lseek_strut*) data);
					strut->clst = getFatValue;
					f_lseek_b_loop_a(strut);
					return;
				}, strut); /* Follow cluster chain if not in write mode */
				return;
			}
			f_lseek_b_loop_a(strut);
			return;
		} else {
			f_lseek_b_loop_end(strut);
			return;
		}
	}

	void f_lseek_b(f_lseek_strut *strut) {
		if (strut->clst != 0) {
			f_lseek_b_loop(strut);
			return;
		}
		f_lseek_c(strut);
		return;
	}

	void f_lseek_a(f_lseek_strut *strut) {
		if (fResult != FR_OK) {
			delete strut;
			callNextCallback();
			return;
		}

#if _NB_USE_FASTSEEK
		if (strut->fp->clusterLinkMapTablePtr) { /* Fast seek */
			if (strut->ofs == CREATE_LINKMAP) { /* Create CLMT */
				strut->tbl = strut->fp->clusterLinkMapTablePtr;
				strut->tlen = *strut->tbl++;
				strut->ulen = 2; /* Given table size and required table size */
				strut->cl = strut->fp->obj.startCluster; /* Origin of the chain */
				if (strut->cl) {
					f_lseek_loop(strut);
					return;
				}
				f_lseek_loop_end(strut);
				return;
			} else { /* Fast seek */
				if (strut->ofs > strut->fp->obj.objSize)
					strut->ofs = strut->fp->obj.objSize; /* Clip offset at the file size */
				strut->fp->fileReadWritePtr = strut->ofs; /* Set file pointer */
				if (strut->ofs) {
					strut->fp->currentFilePtrCluster = clmt_clust(strut->fp, strut->ofs - 1);
					strut->dsc = clust2sect(strut->fs, strut->fp->currentFilePtrCluster);
					if (!strut->dsc) {
						strut->fp->err = FR_INT_ERROR;
						fResult = FR_INT_ERROR;
						delete strut;
						callNextCallback();
						return;
					}
					strut->dsc += (DWORD) ((strut->ofs - 1) / SS(strut->fs)) & (strut->fs->clusterSize - 1);
					if (strut->fp->fileReadWritePtr % SS(strut->fs) && strut->dsc != strut->fp->sectorNumber) { /* Refill sector cache if needed */
#if !_NB_FS_TINY
#if !_NB_FS_READONLY
						if (strut->fp->flag & FA_DIRTY) { /* Write-back dirty sector cache */
							voidPtr = strut;
							disk_write(strut->fs->driveNumber, strut->fp->buf, strut->fp->sectorNumber, 1, [](DRESULT dRes) {
								f_lseek_strut *strut = (f_lseek_strut*) voidPtr;
								if (dRes != RES_OK) {
									strut->fp->err = FR_DISK_ERROR;
									fResult = FR_DISK_ERROR;
									delete strut;
									callNextCallback();
									return;
								}
								strut->fp->flag &= (BYTE) ~FA_DIRTY;
								strut->fp->sectorNumber = strut->dsc;
								delete strut;
								callNextCallback();
								return;
							});
							return;
						}
#endif
						voidPtr = strut;
						disk_read(strut->fs->driveNumber, strut->fp->buf, strut->dsc, 1, [](DRESULT dRes) {
							f_lseek_strut *strut = (f_lseek_strut*) voidPtr;
							if (dRes != RES_OK) {
								strut->fp->err = FR_DISK_ERROR;
								fResult = FR_DISK_ERROR;
								delete strut;
								callNextCallback();
								return;
							} /* Load current sector */
#endif
							strut->fp->sectorNumber = strut->dsc;
							delete strut;
							callNextCallback();
							return;
						});
						return;
					}
				}
			}
		} else
#endif

		/* Normal Seek */
		{
#if _NB_FS_EXFAT
			if (strut->fs->fs_type != FS_EXFAT && strut->ofs >= 0x100000000)
				strut->ofs = 0xFFFFFFFF; /* Clip at 4GiB-1 if at FATxx */
#endif
			if (strut->ofs > strut->fp->obj.objSize && (_NB_FS_READONLY || !(strut->fp->flag & FA_WRITE))) { /* In read-only mode, clip offset with the file size */
				strut->ofs = strut->fp->obj.objSize;
			}
			strut->ifptr = strut->fp->fileReadWritePtr;
			strut->fp->fileReadWritePtr = strut->nsect = 0;
			if (strut->ofs) {
				strut->bcs = (DWORD) strut->fs->clusterSize * SS(strut->fs); /* Cluster size (byte) */
				if (strut->ifptr > 0 && (strut->ofs - 1) / strut->bcs >= (strut->ifptr - 1) / strut->bcs) { /* When seek to same or following cluster, */
					strut->fp->fileReadWritePtr = (strut->ifptr - 1) & ~(FSIZE_t) (strut->bcs - 1); /* start from the current cluster */
					strut->ofs -= strut->fp->fileReadWritePtr;
					strut->clst = strut->fp->currentFilePtrCluster;
				} else { /* When seek to back cluster, */
					strut->clst = strut->fp->obj.startCluster; /* start from the first cluster */
#if !_NB_FS_READONLY
					if (strut->clst == 0) { /* If no cluster chain, create a new chain */
						create_chain(&strut->fp->obj, 0, [](void *data) {
							f_lseek_strut *strut = ((f_lseek_strut*) data);
							strut->clst = createChainValue;
							if (strut->clst == 1) {
								strut->fp->err = FR_INT_ERROR;
								fResult = FR_INT_ERROR;
								delete strut;
								callNextCallback();
								return;
							}
							if (strut->clst == 0xFFFFFFFF) {
								strut->fp->err = FR_DISK_ERROR;
								fResult = FR_DISK_ERROR;
								delete strut;
								callNextCallback();
								return;
							}
							strut->fp->obj.startCluster = strut->clst;
							strut->fp->currentFilePtrCluster = strut->clst;
							f_lseek_b(strut);
							return;
						}, strut);
						return;
					}
#endif
					strut->fp->currentFilePtrCluster = strut->clst;
					f_lseek_b(strut);
					return;
				}
				f_lseek_b(strut);
				return;
			}
			f_lseek_c(strut);
			return;
		}
		delete strut;
		callNextCallback();
		return;
	}

	void f_lseek(FIL *fp, /* Pointer to the file object */
	FSIZE_t ofs /* File pointer from top of file */
	, void (*callback)(void*)) {
		addCallback(callback, &fResult);
		f_lseek_strut *s = new f_lseek_strut();
		s->fp = fp;
		s->ofs = ofs;

		validate(&s->fp->obj, &s->fs, [](void *data) {
			f_lseek_strut *strut = ((f_lseek_strut*) data);
			if (fResult == FR_OK) {
				fResult = (FRESULT) strut->fp->err;
			}
#if _NB_FS_EXFAT && !_NB_FS_READONLY
			if (fResult == FR_OK && strut->fs->fs_type == FS_EXFAT) {
				fill_last_frag(&strut->fp->obj, strut->fp->currentFilePtrCluster, 0xFFFFFFFF, [](void *data) {
					f_lseek_strut *strut = ((f_lseek_strut*) data);
					f_lseek_a(strut);
					return;
				},strut); /* Fill last fragment on the FAT if needed */
				return;
			}
			f_lseek_a(strut);
			return;
#endif
			delete strut;
			callNextCallback();
			return;
		}, s);
		return; /* Check validity of the file object */
	}

#if _NB_FS_MINIMIZE <= 1
	/*-----------------------------------------------------------------------*/
	/* Create a Directory Object                                             */
	/*-----------------------------------------------------------------------*/
	struct f_opendir_strut {
		DIR *dp; /* Pointer to directory object to create */
		const TCHAR *path; /* Pointer to the directory path */
		FATFS *fs;
		_FDID *obj;
	}
	;

	void f_opendir(DIR *dp, /* Pointer to directory object to create */
	const TCHAR *path /* Pointer to the directory path */
	, void (*callback)(void*)) {
		addCallback(callback, &fResult);
		f_opendir_strut *s = new f_opendir_strut();
		s->dp = dp;
		s->path = path;
		DEF_NAMBUF

		if (!s->dp) {
			fResult = FR_INVALID_OBJECT;
			delete s;
			callNextCallback();
			return;
		}

		/* Get logical drive */
		s->obj = &s->dp->obj;
		find_volume(&s->path, &s->fs, 0, [](void *data) {
			f_opendir_strut *strut = ((f_opendir_strut*) data);
			if (fResult == FR_OK) {
				strut->obj->fs = strut->fs;
				INIT_NAMBUF(strut->fs);
				follow_path(strut->dp, strut->path, [](void *data) {
					f_opendir_strut *strut = ((f_opendir_strut*) data);
					if (fResult == FR_OK) { /* Follow completed */
						if (!(strut->dp->fn[NSFLAG] & NS_NONAME)) { /* It is not the origin directory itself */
							if (strut->obj->attr & AM_DIRECTORY) { /* This object is a sub-directory */
#if _NB_FS_EXFAT
					if (strut->fs->fs_type == FS_EXFAT) {
						strut->obj->containingDirectoryStartCluster = strut->obj->startCluster; /* Get containing directory inforamation */
						strut->obj->sizeOfContainingDirectory = ((DWORD) strut->obj->objSize & 0xFFFFFF00) | strut->obj->stat;
						strut->obj->offsetInContainingDirectory = strut->dp->currentEntryBlockOffset;
						strut->obj->startCluster = ld_dword(strut->fs->dirBuffer + XDIR_FstClus); /* Get object allocation info */
						strut->obj->objSize = ld_qword(strut->fs->dirBuffer + XDIR_FileSize);
						strut->obj->stat = strut->fs->dirBuffer[XDIR_GenFlags] & 2;
					} else
#endif
					{
						strut->obj->startCluster = ld_clust(strut->fs, strut->dp->dirPtr); /* Get object allocation info */
					}
				} else { /* This object is a file */
					fResult = FR_NO_PATH;
				}
			}
			if (fResult == FR_OK) {
				strut->obj->id = strut->fs->id;
				dir_setDirectoryIndex(strut->dp, 0, [](void *data) {
					f_opendir_strut *strut = ((f_opendir_strut*) data);
#if _NB_FS_LOCK != 0
					if (fResult == FR_OK) {
						if (strut->obj->startCluster) {
							strut->obj->lockid = inc_lock(strut->dp, 0); /* Lock the sub directory */
							if (!strut->obj->lockid) {
								fResult = FR_TOO_MANY_OPEN_FILES;
							}
						} else {
							strut->obj->lockid = 0; /* Root directory need not to be locked */
						}
					}
#endif
					FREE_NAMBUF();
					if (fResult == FR_NO_FILE) {
						fResult = FR_NO_PATH;
					}
					if (fResult != FR_OK) {
						strut->obj->fs = 0;
					} /* Invalidate the directory object if function faild */
					delete strut;
					callNextCallback();
					return;
				},strut); /* Rewind directory */
				return;
			}
		} FREE_NAMBUF() ;
		if (fResult == FR_NO_FILE) {
			fResult = FR_NO_PATH;
		}
		if (fResult != FR_OK) {
			strut->obj->fs = 0;
		} /* Invalidate the directory object if function faild */
		delete strut;
		callNextCallback();
		return;
	}			,strut); /* Follow the path to the directory */
				return;
			}
			if (fResult != FR_OK) {
				strut->obj->fs = 0;
			} /* Invalidate the directory object if function faild */
			delete strut;
			callNextCallback();
			return;
		}, s);
		return;
	}

	/*-----------------------------------------------------------------------*/
	/* Close Directory                                                       */
	/*-----------------------------------------------------------------------*/

	struct f_closedir_strut {
		DIR *dp;
		FATFS *fs;
	}
	;

	void f_closedir(DIR *dp /* Pointer to the directory object to be closed */
	, void (*callback)(void*)) {
		addCallback(callback, &fResult);
		f_closedir_strut *s = new f_closedir_strut();
		s->dp = dp;

		validate(&s->dp->obj, &s->fs, [](void *data) {
			f_closedir_strut *strut = ((f_closedir_strut*) data);
			if (fResult == FR_OK) {
#if _NB_FS_LOCK != 0
			if (strut->dp->obj.lockid) { /* Decrement sub-directory open counter */
				fResult = dec_lock(strut->dp->obj.lockid);
			}
			if (fResult == FR_OK)
#endif
					{
				strut->dp->obj.fs = 0; /* Invalidate directory object */
			}
#if _NB_FS_REENTRANT
#error
					 unlock_fs(fs, FR_OK); /* Unlock volume */
#endif
		}
		delete strut;
		callNextCallback();
		return;
	}, s);
		return; /* Check validity of the file object */
	}

	/*-----------------------------------------------------------------------*/
	/* Read Directory Entries in Sequence                                    */
	/*-----------------------------------------------------------------------*/
	struct f_readdir_strut {
		DIR *dp; /* Pointer to the open directory object */
		FILINFO *fno; /* Pointer to file information to return */
		FATFS *fs;
	}
	;

	void f_readdir(DIR *dp, /* Pointer to the open directory object */
	FILINFO *fno /* Pointer to file information to return */
	, void (*callback)(void*)) {
		addCallback(callback, &fResult);
		f_readdir_strut *s = new f_readdir_strut();
		s->dp = dp;
		s->fno = fno;
		DEF_NAMBUF

		validate(&s->dp->obj, &s->fs, [](void *data) {
			f_readdir_strut *strut = ((f_readdir_strut*) data);
			if (fResult == FR_OK) {
				if (!strut->fno) {
					dir_setDirectoryIndex(strut->dp, 0, [](void *data) {
						f_readdir_strut *strut = ((f_readdir_strut*) data);
						delete strut;
						callNextCallback();
						return;
					},strut); /* Rewind the directory object */
					return;
				} else {
					INIT_NAMBUF(strut->fs);
					dir_read(strut->dp, 0, [](void *data) {
						f_readdir_strut *strut = ((f_readdir_strut*) data);
						if (fResult == FR_NO_FILE) {
							fResult = FR_OK;
						} /* Ignore end of directory */
						if (fResult == FR_OK) { /* A valid entry is found */
							get_fileinfo(strut->dp, strut->fno); /* Get the object information */
							dir_next(strut->dp, 0, [](void *data) {
								f_readdir_strut *strut = ((f_readdir_strut*) data); /* Increment index for next */
								if (fResult == FR_NO_FILE) {
									fResult = FR_OK;
								} /* Ignore end of directory now */
								FREE_NAMBUF();
								delete strut;
								callNextCallback();
								return;
							},strut);
							return;
						}FREE_NAMBUF();
						delete strut;
						callNextCallback();
						return;
					},strut); /* Read an item */
					return;
				}
			}
			delete strut;
			callNextCallback();
			return;
		}, s);
		return; /* Check validity of the directory object */
	}

#if _NB_USE_FIND
#error
/*-----------------------------------------------------------------------*/
/* Find Next File                                                        */
/*-----------------------------------------------------------------------*/

FRESULT f_findnext(
	DIR *dp,	 /* Pointer to the open directory object */
	FILINFO *fno /* Pointer to the file information structure */
	,
	void (*callback)(void *))
{
	addCallback(callback, data);
	FRESULT res;

	for (;;)
	{
		res = f_readdir(dp, fno); /* Get a directory item */
		if (res != FR_OK || !fno || !fno->fname[0])
			break; /* Terminate if any error or end of directory */
		if (pattern_matching(dp->pat, fno->fname, 0, 0))
			break; /* Test for the file name */
#if _NB_USE_LFN != 0 && _NB_USE_FIND == 2
		if (pattern_matching(dp->pat, fno->altname, 0, 0))
			break; /* Test for alternative name if exist */
#endif
	}
	return res;
}

/*-----------------------------------------------------------------------*/
/* Find First File                                                       */
/*-----------------------------------------------------------------------*/

FRESULT f_findfirst(
	DIR *dp,			 /* Pointer to the blank directory object */
	FILINFO *fno,		 /* Pointer to the file information structure */
	const TCHAR *path,	 /* Pointer to the directory to open */
	const TCHAR *pattern /* Pointer to the matching pattern */
	,
	void (*callback)(void *))
{
	addCallback(callback, data);
	FRESULT res;

	dp->pat = pattern;		   /* Save pointer to pattern string */
	res = f_opendir(dp, path); /* Open the target directory */
	if (res == FR_OK)
	{
		res = f_findnext(dp, fno); /* Find the first item */
	}
	return res;
}

#endif /* _USE_FIND */

#if _NB_FS_MINIMIZE == 0
	/*-----------------------------------------------------------------------*/
	/* Get File Status                                                       */
	/*-----------------------------------------------------------------------*/
	struct f_stat_strut {
		const TCHAR *path; /* Pointer to the file path */
		FILINFO *fno; /* Pointer to file information to return */
		DIR dj;
	};

	void f_stat(const TCHAR *path, /* Pointer to the file path */
	FILINFO *fno /* Pointer to file information to return */
	, void (*callback)(void*)) {
		addCallback(callback, &fResult);
		f_stat_strut *s = new f_stat_strut();
		DEF_NAMBUF
		s->fno = fno;
		s->path = path;

		// /* Get logical drive */
		find_volume(&s->path, &s->dj.obj.fs, 0, [](void *data) {
			f_stat_strut *strut = ((f_stat_strut*) data);
			if (fResult == FR_OK) {
				INIT_NAMBUF(strut->dj.obj.strut->fs);
				follow_path(&strut->dj, strut->path, [](void *data) {
					f_stat_strut *strut = ((f_stat_strut*) data);
					if (fResult == FR_OK) { /* Follow completed */
						if (strut->dj.fn[NSFLAG] & NS_NONAME) { /* It is origin directory */
							fResult = FR_INVALID_NAME;
						} else { /* Found an object */
							if (strut->fno) {
								get_fileinfo(&strut->dj, strut->fno);
							}
						}
					}FREE_NAMBUF();
					delete strut;
					callNextCallback();
					return;
				}, strut); /* Follow the file path */
				return;
			}
			delete strut;
			callNextCallback();
			return;
		}, s);
		return;
	}

#if !_NB_FS_READONLY
	/*-----------------------------------------------------------------------*/
	/* Get Number of Free Clusters                                           */
	/*-----------------------------------------------------------------------*/
	struct f_getfree_strut {
		const TCHAR *path; /* Path name of the logical drive number */
		DWORD *nclst; /* Pointer to a variable to return number of free clusters */
		FATFS **fatfs; /* Pointer to return pointer to corresponding file system object */
		FATFS *fs;
		DWORD nfree, clst, sect, stat;
		UINT i;
		BYTE *p;
		_FDID obj;
		BYTE bm;
		UINT b;
	};

	void f_getfree_end(f_getfree_strut *strut) {

		*strut->nclst = strut->nfree; /* Return the free clusters */
		strut->fs->numberOfFreeClusters = strut->nfree; /* Now numberOfFreeClusters is valid */
		strut->fs->fsi_flag |= 1; /* FSInfo is to be updated */
		delete strut;
		callNextCallback();
		return;
	}
	void f_getfree_a_loopb(f_getfree_strut *strut);
	void f_getfree_a_loopb_a(f_getfree_strut *strut) {
		if (strut->fs->fs_type == FS_FAT16) {
			if (ld_word(strut->p) == 0) {
				strut->nfree++;
			}
			strut->p += 2;
			strut->i -= 2;
		} else {
			if ((ld_dword(strut->p) & 0x0FFFFFFF) == 0) {
				strut->nfree++;
			}
			strut->p += 4;
			strut->i -= 4;
		}
		if (--strut->clst) {
			f_getfree_a_loopb(strut);
			return;
		} else {
			f_getfree_end(strut);
			return;
		}
	}

	void f_getfree_a_loopb(f_getfree_strut *strut) {
		if (strut->i == 0) {
			move_window(strut->fs, strut->sect++, [](void *data) {
				f_getfree_strut *strut = ((f_getfree_strut*) data);
				if (fResult != FR_OK) {
					f_getfree_end(strut);
					return;
				}
				strut->p = strut->fs->win;
				strut->i = SS(strut->fs);
				f_getfree_a_loopb_a(strut);
				return;
			}, strut);
			return;
		}
		f_getfree_a_loopb_a(strut);
		return;
	}

	void f_getfree_a_loop(f_getfree_strut *strut) {
		move_window(strut->fs, strut->sect++, [](void *data) {
			f_getfree_strut *strut = ((f_getfree_strut*) data);
			if (strut->i == 0 && fResult != FR_OK) {
				f_getfree_end(strut);
				return;
			}
			for (strut->b = 8, strut->bm = strut->fs->win[strut->i]; strut->b && strut->clst; strut->b--, strut->clst--) {
				if (!(strut->bm & 1)) {
					strut->nfree++;
				}
				strut->bm >>= 1;
			}
			strut->i = (strut->i + 1) % SS(strut->fs);

			if (strut->clst) {
				f_getfree_a_loop(strut);
				return;
			} else {
				f_getfree_end(strut);
				return;
			}
		}, strut);
		return;
	}

	void f_getfree_a(f_getfree_strut *strut) {
#if _NB_FS_EXFAT
		if (strut->fs->fs_type == FS_EXFAT) { /* exFAT: Scan bitmap table */

			strut->clst = strut->fs->numberOfFatEntries - 2;
			strut->sect = strut->fs->dataBaseSector;
			strut->i = 0;
			f_getfree_a_loop(strut);
			return;
		} else
#endif
		{ /* FAT16/32: Sector alighed FAT entries */
			strut->clst = strut->fs->numberOfFatEntries;
			strut->sect = strut->fs->fatBaseSector;
			strut->i = 0;
			strut->p = 0;
			f_getfree_a_loopb(strut);
			return;
		}
	}

	void f_getfree_loop(f_getfree_strut *strut) {
		get_fat(&strut->obj, strut->clst, [](void *data) {
			f_getfree_strut *strut = ((f_getfree_strut*) data);
			strut->stat = getFatValue;
			if (strut->stat == 0xFFFFFFFF) {
				fResult = FR_DISK_ERROR;
				f_getfree_end(strut);
				return;
			}
			if (strut->stat == 1) {
				fResult = FR_INT_ERROR;
				f_getfree_end(strut);
				return;
			}
			if (strut->stat == 0) {
				strut->nfree++;
			}
			if (++strut->clst < strut->fs->numberOfFatEntries) {
				f_getfree_loop(strut);
				return;
			} else {
				f_getfree_end(strut);
				return;
			}
		}, strut);
		return;
	}

	void f_getfree(const TCHAR *path, /* Path name of the logical drive number */
	DWORD *nclst, /* Pointer to a variable to return number of free clusters */
	FATFS **fatfs /* Pointer to return pointer to corresponding file system object */
	, void (*callback)(void*)) {
		addCallback(callback, &fResult);
		f_getfree_strut *s = new f_getfree_strut();
		s->path = path;
		s->nclst = nclst;
		s->fs = *fatfs;

		/* Get logical drive */
		find_volume(&s->path, &s->fs, 0, [](void *data) {
			f_getfree_strut *strut = ((f_getfree_strut*) data);
			if (fResult == FR_OK) { /* Return ptr to the fs object */
				/* If numberOfFreeClusters is valid, return it without full cluster scan */
				if (strut->fs->numberOfFreeClusters <= strut->fs->numberOfFatEntries - 2) {
					*strut->nclst = strut->fs->numberOfFreeClusters;
				} else {
					/* Get number of free clusters */
					strut->nfree = 0;
					if (strut->fs->fs_type == FS_FAT12) { /* FAT12: Sector unalighed FAT entries */
						strut->clst = 2;
						strut->obj.fs = strut->fs;
						f_getfree_loop(strut);
						return;
					} else {
						f_getfree_a(strut);
						return;
					}
				}
			}
			delete strut;
			callNextCallback();
			return;
		}, s);
		return;
	}

	/*-----------------------------------------------------------------------*/
	/* Truncate File                                                         */
	/*-----------------------------------------------------------------------*/
	struct f_truncate_strut {
		FIL *fp; /* Pointer to the file object */
		FATFS *fs;
		DWORD ncl;
	}
	;

	void f_truncate_a(f_truncate_strut *strut) {

		if (fResult != FR_OK) {
			strut->fp->err = fResult;
			delete strut;
			callNextCallback();
			return;
		}
		delete strut;
		callNextCallback();
		return;
	}

	void f_truncate_b(f_truncate_strut *strut) {
		strut->fp->obj.objSize = strut->fp->fileReadWritePtr; /* Set file size to current R/W point */
		strut->fp->flag |= FA_MODIFIED;
#if !_NB_FS_TINY
		if (fResult == FR_OK && (strut->fp->flag & FA_DIRTY)) {
			voidPtr = strut;
			disk_write(strut->fs->driveNumber, strut->fp->buf, strut->fp->sectorNumber, 1, [](DRESULT dRes) {
				f_truncate_strut *strut = (f_truncate_strut*) voidPtr;
				if (dRes != RES_OK) {
					fResult = FR_DISK_ERROR;
				} else {
					strut->fp->flag &= (BYTE) ~FA_DIRTY;
				}
				f_truncate_a(strut);
				return;
			});
			return;
		}
#endif
		f_truncate_a(strut);
		return;
	}

	void f_truncate(FIL *fp /* Pointer to the file object */
	, void (*callback)(void*)) {
		addCallback(callback, &fResult);
		f_truncate_strut *s = new f_truncate_strut();
		s->fp = fp;

		validate(&s->fp->obj, &s->fs, [](void *data) {
			f_truncate_strut *strut = ((f_truncate_strut*) data);
			if (fResult != FR_OK || (fResult = (FRESULT) strut->fp->err) != FR_OK) {
				delete strut;
				callNextCallback();
				return;
			}
			if (!(strut->fp->flag & FA_WRITE)) {
				fResult = FR_DENIED;
				delete strut;
				callNextCallback();
				return;
			} /* Check access mode */

			if (strut->fp->fileReadWritePtr < strut->fp->obj.objSize) { /* Process when fileReadWritePtr is not on the eof */
				if (strut->fp->fileReadWritePtr == 0) { /* When set file size to zero, remove entire cluster chain */
					remove_chain(&strut->fp->obj, strut->fp->obj.startCluster, 0, [](void *data) {
						f_truncate_strut *strut = ((f_truncate_strut*) data);
						strut->fp->obj.startCluster = 0;
						f_truncate_b(strut);
						return;
					},strut);
					return;
				} else { /* When truncate a part of the file, remove remaining clusters */
					get_fat(&strut->fp->obj, strut->fp->currentFilePtrCluster, [](void *data) {
						f_truncate_strut *strut = ((f_truncate_strut*) data);
						strut->ncl = getFatValue;
						fResult = FR_OK;
						if (strut->ncl == 0xFFFFFFFF) {
							fResult = FR_DISK_ERROR;
						}
						if (strut->ncl == 1) {
							fResult = FR_INT_ERROR;
						}
						if (fResult == FR_OK && strut->ncl < strut->fs->numberOfFatEntries) {
							remove_chain(&strut->fp->obj, strut->ncl, strut->fp->currentFilePtrCluster, [](void *data) {
								f_truncate_strut *strut = ((f_truncate_strut*) data);
								f_truncate_b(strut);
								return;
							},strut);
							return;
						}
					},strut);
					f_truncate_b(strut);
					return;
				}
				f_truncate_b(strut);
				return;
			}
			delete strut;
			callNextCallback();
			return;
		}, s);
		return; /* Check validity of the file object */
	}

	/*-----------------------------------------------------------------------*/
	/* Delete a File/Directory                                               */
	/*-----------------------------------------------------------------------*/
	struct f_unlink_strut {
		const TCHAR *path; /* Pointer to the file or directory path */
		DIR dj, sdj;
		DWORD dclst = 0;
		FATFS *fs;
#if _NB_FS_EXFAT
		_FDID obj;
#endif
	}
	;

	void f_unlink_a(f_unlink_strut *strut) {
		if (fResult == FR_OK) {
			dir_remove(&strut->dj, [](void *data) {
				f_unlink_strut *strut = ((f_unlink_strut*) data);

				if (fResult == FR_OK && strut->dclst) { /* Remove the cluster chain if exist */
#if _NB_FS_EXFAT
				remove_chain(&strut->obj, strut->dclst, 0, [](void *data) {
					f_unlink_strut *strut = ((f_unlink_strut*) data);
					if (fResult == FR_OK) {
						sync_fs(strut->fs, [](void *data) {
							f_unlink_strut *strut = ((f_unlink_strut*) data);
							FREE_NAMBUF();
							delete strut;
							callNextCallback();
							return;
						}, strut);
						return;
					}FREE_NAMBUF();
					delete strut;
					callNextCallback();
					return;
				}, strut);
				return;
#else
								remove_chain(&strut->dj.obj, dclst, 0);
#endif
			}
			if (fResult == FR_OK) {
				sync_fs(strut->fs, [](void *data) {
					f_unlink_strut *strut = ((f_unlink_strut*) data);
					FREE_NAMBUF();
					delete strut;
					callNextCallback();
					return;
				}, strut);
				return;
			}FREE_NAMBUF();
			delete strut;
			callNextCallback();
			return;
		}	, strut); /* Remove the directory entry */
			return;
		}FREE_NAMBUF();
		delete strut;
		callNextCallback();
	}

	void f_unlink(const TCHAR *path /* Pointer to the file or directory path */
	, void (*callback)(void*)) {
		addCallback(callback, &fResult);
		f_unlink_strut *s = new f_unlink_strut();
		s->path = path;
		s->dclst = 0;
		DEF_NAMBUF

		// 	/* Get logical drive */
		find_volume(&s->path, &s->fs, FA_WRITE, [](void *data) {
			f_unlink_strut *strut = ((f_unlink_strut*) data);
			strut->dj.obj.fs = strut->fs;
			if (fResult == FR_OK) {
				INIT_NAMBUF(strut->fs);
				follow_path(&strut->dj, strut->path, [](void *data) {
					f_unlink_strut *strut = ((f_unlink_strut*) data);
					if (_NB_FS_RPATH && fResult == FR_OK && (strut->dj.fn[NSFLAG] & NS_DOT)) {
						fResult = FR_INVALID_NAME; /* Cannot remove dot entry */
					}
#if _NB_FS_LOCK != 0
					if (fResult == FR_OK) {
						fResult = chk_lock(&strut->dj, 2);
					} /* Check if it is an open object */
#endif
					if (fResult == FR_OK) { /* The object is accessible */
						if (strut->dj.fn[NSFLAG] & NS_NONAME) {
							fResult = FR_INVALID_NAME; /* Cannot remove the origin directory */
						} else {
							if (strut->dj.obj.attr & AM_READ_ONLY) {
								fResult = FR_DENIED; /* Cannot remove R/O object */
							}
						}
						if (fResult == FR_OK) {
#if _NB_FS_EXFAT
					strut->obj.fs = strut->fs;
					if (strut->fs->fs_type == FS_EXFAT) {
						strut->obj.startCluster = strut->dclst = ld_dword(strut->fs->dirBuffer + XDIR_FstClus);
						strut->obj.objSize = ld_qword(strut->fs->dirBuffer + XDIR_FileSize);
						strut->obj.stat = strut->fs->dirBuffer[XDIR_GenFlags] & 2;
					} else
#endif
					{
						strut->dclst = ld_clust(strut->fs, strut->dj.dirPtr);
					}
					if (strut->dj.obj.attr & AM_DIRECTORY) { /* Is it a sub-directory? */
#if _NB_FS_RPATH != 0
													if (dclst == strut->fs->cdir)
													{ /* Is it the current directory? */
														fResult = FR_DENIED;
													}
													else
#endif
					{
						strut->sdj.obj.fs = strut->fs; /* Open the sub-directory */
						strut->sdj.obj.startCluster = strut->dclst;
#if _NB_FS_EXFAT
					if (strut->fs->fs_type == FS_EXFAT) {
						strut->sdj.obj.objSize = strut->obj.objSize;
						strut->sdj.obj.stat = strut->obj.stat;
					}
#endif
					dir_setDirectoryIndex(&strut->sdj, 0, [](void *data) {
						f_unlink_strut *strut = ((f_unlink_strut*) data);
						if (fResult == FR_OK) {
							dir_read(&strut->sdj, 0, [](void *data) {
								f_unlink_strut *strut = ((f_unlink_strut*) data);
								if (fResult == FR_OK) {
									fResult = FR_DENIED;
								} /* Not empty? */
								if (fResult == FR_NO_FILE) {
									fResult = FR_OK;
								} /* Empty? */
								f_unlink_a(strut);
								return;
							},strut); /* Read an item */
							return;
						}
						f_unlink_a(strut);
						return;
					},strut);
					return;
				}
			}
		}
		f_unlink_a(strut);
		return;
	}FREE_NAMBUF();
	delete strut;
	callNextCallback();
	return;
}, strut); /* Follow the file path */
				return;
			}
			delete strut;
			callNextCallback();
			return;
		}, s);
		return;
	}

	/*-----------------------------------------------------------------------*/
	/* Create a Directory                                                    */
	/*-----------------------------------------------------------------------*/
	struct f_mkdir_strut {
		const TCHAR *path; /* Pointer to the directory path */
		DIR dj;
		FATFS *fs;
		BYTE *dir;
		UINT n;
		DWORD dsc, dcl, pcl, tm;
	}
	;

	void f_mkdir_c(f_mkdir_strut *strut) {
		if (fResult == FR_OK) {
			sync_fs(strut->fs, [](void *data) {
				f_mkdir_strut *strut = ((f_mkdir_strut*) data);
				FREE_NAMBUF();
				delete strut;
				callNextCallback();
				return;
			}, strut);
			return;
		}FREE_NAMBUF();
		delete strut;
		callNextCallback();
		return;
	}

	void f_mkdir_b(f_mkdir_strut *strut) {
		if (fResult == FR_OK) {
#if _NB_FS_EXFAT
			if (strut->fs->fs_type == FS_EXFAT) { /* Initialize directory entry block */
				st_dword(strut->fs->dirBuffer + XDIR_ModTime, strut->tm); /* Created time */
				st_dword(strut->fs->dirBuffer + XDIR_FstClus, strut->dcl); /* Table start cluster */
				st_dword(strut->fs->dirBuffer + XDIR_FileSize, (DWORD) strut->dj.obj.objSize); /* File size needs to be valid */
				st_dword(strut->fs->dirBuffer + XDIR_ValidFileSize, (DWORD) strut->dj.obj.objSize);
				strut->fs->dirBuffer[XDIR_GenFlags] = 3; /* Initialize the object flag (contiguous) */
				strut->fs->dirBuffer[XDIR_Attr] = AM_DIRECTORY; /* Attribute */
				store_xdir(&strut->dj, [](void *data) {
					f_mkdir_strut *strut = ((f_mkdir_strut*) data);
					f_mkdir_c(strut);
					return;
				}, strut);
				return;
			} else
#endif
			{
				strut->dir = strut->dj.dirPtr;
				st_dword(strut->dir + DIR_ModTime, strut->tm); /* Created time */
				st_clust(strut->fs, strut->dir, strut->dcl); /* Table start cluster */
				strut->dir[DIR_Attr] = AM_DIRECTORY; /* Attribute */
				strut->fs->wflag = 1;
				f_mkdir_c(strut);
				return;
			}
		} else {
			remove_chain(&strut->dj.obj, strut->dcl, 0, [](void *data) {
				f_mkdir_strut *strut = ((f_mkdir_strut*) data);
				FREE_NAMBUF();
				delete strut;
				callNextCallback();
			}, strut); /* Could not register, remove cluster chain */
			return;
		}
	}

	void f_mkdir_a(f_mkdir_strut *strut) {
		if (fResult == FR_OK) {
			dir_register(&strut->dj, [](void *data) {
				f_mkdir_strut *strut = ((f_mkdir_strut*) data);
				f_mkdir_b(strut);
				return;
			}, strut); /* Register the object to the directoy */
			return;
		}
		f_mkdir_b(strut);
		return;
	}

	void f_mkdir_loop(f_mkdir_strut *strut) {
		/* Write dot entries and clear following sectors */
		strut->fs->winSector = strut->dsc++;
		strut->fs->wflag = 1;
		sync_window(strut->fs, [](void *data) {
			f_mkdir_strut *strut = ((f_mkdir_strut*) data);
			if (fResult != FR_OK) {
				f_mkdir_a(strut);
				return;
			}
			mem_set(strut->dir, 0, SS(strut->fs));
			strut->n--;
			if (strut->n) {
				f_mkdir_loop(strut);
				return;
			} else {
				f_mkdir_a(strut);
				return;
			}
		}, strut);
		return;
	}

	void f_mkdir(const TCHAR *path /* Pointer to the directory path */
	, void (*callback)(void*)) {
		addCallback(callback, &fResult);
		f_mkdir_strut *s = new f_mkdir_strut();
		s->path = path;
		DEF_NAMBUF

		/* Get logical drive */
		find_volume(&s->path, &s->fs, FA_WRITE, [](void *data) {
			f_mkdir_strut *strut = ((f_mkdir_strut*) data);
			strut->dj.obj.fs = strut->fs;
			if (fResult == FR_OK) {
				INIT_NAMBUF(strut->fs);
				follow_path(&strut->dj, strut->path, [](void *data) {
					f_mkdir_strut *strut = ((f_mkdir_strut*) data);
					if (fResult == FR_OK) {
						fResult = FR_EXIST;
					} /* Any object with same name is already existing */
					if (_NB_FS_RPATH && fResult == FR_NO_FILE && (strut->dj.fn[NSFLAG] & NS_DOT)) {
						fResult = FR_INVALID_NAME;
					}
					if (fResult == FR_NO_FILE) { /* Can create a new directory */
						create_chain(&strut->dj.obj, 0, [](void *data) {
							f_mkdir_strut *strut = ((f_mkdir_strut*) data);
							strut->dcl = createChainValue;
							strut->dj.obj.objSize = (DWORD) strut->fs->clusterSize * SS(strut->fs);
							fResult = FR_OK;
							if (strut->dcl == 0) {
								fResult = FR_DENIED;
							} /* No space to allocate a new cluster */
							if (strut->dcl == 1) {
								fResult = FR_INT_ERROR;
							}
							if (strut->dcl == 0xFFFFFFFF) {
								fResult = FR_DISK_ERROR;
							}
							if (fResult == FR_OK) {
								sync_window(strut->fs, [](void *data) {
									f_mkdir_strut *strut = ((f_mkdir_strut*) data);
									/* Flush FAT */
									strut->tm = GET_FATTIME();
									if (fResult == FR_OK) { /* Initialize the new directory table */
										strut->dsc = clust2sect(strut->fs, strut->dcl);
										strut->dir = strut->fs->win;
										mem_set(strut->dir, 0, SS(strut->fs));
										if (!_NB_FS_EXFAT || strut->fs->fs_type != FS_EXFAT) {
											mem_set(strut->dir + DIR_Name, ' ', 11); /* Create "." entry */
											strut->dir[DIR_Name] = '.';
											strut->dir[DIR_Attr] =
											AM_DIRECTORY;
											st_dword(strut->dir + DIR_ModTime, strut->tm);
											st_clust(strut->fs, strut->dir, strut->dcl);
											mem_cpy(strut->dir + SZDIRE, strut->dir, SZDIRE); /* Create ".." entry */
											strut->dir[SZDIRE + 1] = '.';
											strut->pcl = strut->dj.obj.startCluster;
											if (strut->fs->fs_type == FS_FAT32 && strut->pcl == strut->fs->dirBaseSector) {
												strut->pcl = 0;
											}
											st_clust(strut->fs, strut->dir + SZDIRE, strut->pcl);
										}
										strut->n = strut->fs->clusterSize;
										f_mkdir_loop(strut);
										return;
									}
								},strut);
								return;
							}
							strut->tm = GET_FATTIME();
							f_mkdir_a(strut);
							return;
						},strut); /* Allocate a cluster for the new directory table */
						return;
					} FREE_NAMBUF();
					delete strut;
					callNextCallback();
					return;
				},strut); /* Follow the file path */
				return;
			}
			delete strut;
			callNextCallback();
			return;
		}, s);
		return;
	}

	/*-----------------------------------------------------------------------*/
	/* Rename a File/Directory                                               */
	/*-----------------------------------------------------------------------*/
	struct f_rename_strut {
		const TCHAR *path_old; /* Pointer to the object name to be renamed */
		const TCHAR *path_new; /* Pointer to the new name */
		FRESULT res;
		DIR djo, djn;
		FATFS *fs;
		BYTE buf[_NB_FS_EXFAT ? SZDIRE * 2 : 24], *dir;
		DWORD dw;
	}
	;

	void f_rename_a(f_rename_strut *strut) {
		if (fResult == FR_OK) {
			dir_remove(&strut->djo, [](void *data) {
				f_rename_strut *strut = ((f_rename_strut*) data);
				if (fResult == FR_OK) {
					sync_fs(strut->fs, [](void *data) {
						f_rename_strut *strut = ((f_rename_strut*) data);
						FREE_NAMBUF();
						delete strut;
						callNextCallback();
						return;
					},strut);
					return;
				}FREE_NAMBUF();
				delete strut;
				callNextCallback();
				return;
			}, strut); /* Remove old entry */
			return;
		}FREE_NAMBUF();
		delete strut;
		callNextCallback();
		return;
		/* End of the critical section */
	}

	void f_rename(const TCHAR *path_oldInput, /* Pointer to the object name to be renamed */
	const TCHAR *path_newInput /* Pointer to the new name */
	, void (*callback)(void*)) {
		addCallback(callback, &fResult);
		f_rename_strut *s = new f_rename_strut();
		s->path_old = path_oldInput;
		s->path_new = path_newInput;
		DEF_NAMBUF

		get_ldnumber(&s->path_new); /* Snip drive number of new name off */
		find_volume(&s->path_old, &s->fs, FA_WRITE, [](void *data) {
			f_rename_strut *strut = ((f_rename_strut*) data);
			if (fResult == FR_OK) {
				strut->djo.obj.fs = strut->fs;
				INIT_NAMBUF(strut->fs);
				follow_path(&strut->djo, strut->path_old, [](void *data) {
					f_rename_strut *strut = ((f_rename_strut*) data);
					if (fResult == FR_OK && (strut->djo.fn[NSFLAG] & (NS_DOT | NS_NONAME))) {
						fResult = FR_INVALID_NAME;
					} /* Check validity of name */
#if _NB_FS_LOCK != 0
					if (fResult == FR_OK) {
						fResult = chk_lock(&strut->djo, 2);
					}
#endif
					if (fResult == FR_OK) { /* Object to be renamed is found */
#if _NB_FS_EXFAT
					if (strut->fs->fs_type == FS_EXFAT) { /* At exFAT */

						mem_cpy(&strut->buf, strut->fs->dirBuffer, SZDIRE * 2); /* Save 85+C0 entry of old object */
						mem_cpy(&strut->djn, &strut->djo, sizeof(DIR));
						follow_path(&strut->djn, strut->path_new, [](void *data) {
							f_rename_strut *strut = ((f_rename_strut*) data);
							if (fResult == FR_OK) { /* Is new name already in use by any other object? */
								fResult = (strut->djn.obj.startCluster == strut->djo.obj.startCluster && strut->djn.currentReadWriteOffset == strut->djo.currentReadWriteOffset) ?
		FR_NO_FILE : FR_EXIST;
	}
							if (fResult == FR_NO_FILE) { /* It is a valid path and no name collision */
								dir_register(&strut->djn, [](void *data) {
									f_rename_strut *strut = ((f_rename_strut*) data);
									if (fResult == FR_OK) {
										BYTE nf, nn;
										WORD nh;
										nf = strut->fs->dirBuffer[XDIR_NumSec];
										nn = strut->fs->dirBuffer[XDIR_NumName];
										nh = ld_word(strut->fs->dirBuffer + XDIR_NameHash);
										mem_cpy(strut->fs->dirBuffer, strut->buf,
										SZDIRE * 2);
										strut->fs->dirBuffer[XDIR_NumSec] = nf;
										strut->fs->dirBuffer[XDIR_NumName] = nn;
										st_word(strut->fs->dirBuffer + XDIR_NameHash, nh);
										/* Start of critical section where an interruption can cause a cross-link */
										store_xdir(&strut->djn, [](void *data) {
											f_rename_strut *strut = ((f_rename_strut*) data);
											f_rename_a(strut);
											return;
										},strut);
										return;
									}
									f_rename_a(strut);
									return;
								},strut); /* Register the new entry */
								return;
							}
							f_rename_a(strut);
							return;
						},strut); /* Make sure if new object name is not in use */
						return;
					} else
#endif
					{ /* At FAT12/FAT16/FAT32 */
						mem_cpy(&strut->buf, strut->djo.dirPtr + DIR_Attr, 21); /* Save information about the object except name */
						mem_cpy(&strut->djn, &strut->djo, sizeof(DIR)); /* Duplicate the directory object */
						follow_path(&strut->djn, strut->path_new, [](void *data) {
							f_rename_strut *strut = ((f_rename_strut*) data);
							if (fResult == FR_OK) { /* Is new name already in use by any other object? */
								fResult = (strut->djn.obj.startCluster == strut->djo.obj.startCluster && strut->djn.currentReadWriteOffset == strut->djo.currentReadWriteOffset) ?
						FR_NO_FILE : FR_EXIST;
					}
							if (fResult == FR_NO_FILE) { /* It is a valid path and no name collision */
								dir_register(&strut->djn, [](void *data) {
									f_rename_strut *strut = ((f_rename_strut*) data);
									if (fResult == FR_OK) {
										strut->dir = strut->djn.dirPtr; /* Copy information about object except name */
										mem_cpy(strut->dir + 13, strut->buf + 2, 19);
										strut->dir[DIR_Attr] = ((BYTE*) strut->buf)[0] | AM_ARCHIVE;
										strut->fs->wflag = 1;
										if ((strut->dir[DIR_Attr] & AM_DIRECTORY) && strut->djo.obj.startCluster != strut->djn.obj.startCluster) { /* Update .. entry in the sub-directory if needed */
											strut->dw = clust2sect(strut->fs, ld_clust(strut->fs, strut->dir));
											if (!strut->dw) {
												fResult = FR_INT_ERROR;
											} else {
												/* Start of critical section where an interruption can cause a cross-link */
												move_window(strut->fs, strut->dw, [](void *data) {
													f_rename_strut *strut = ((f_rename_strut*) data);
													strut->dir = strut->fs->win + SZDIRE * 1; /* Ptr to .. entry */
													if (fResult == FR_OK && strut->dir[1] == '.') {
														st_clust(strut->fs, strut->dir, strut->djn.obj.startCluster);
														strut->fs->wflag = 1;
													}
													f_rename_a(strut);
													return;
												} ,strut);
												return;
											}
										}
									}
									f_rename_a(strut);
									return;
								},strut); /* Register the new entry */
								return;
							}
							f_rename_a(strut);
							return;
						},strut); /* Make sure if new object name is not in use */
						return;
					}
				}FREE_NAMBUF();
				delete strut;
				callNextCallback();
				return;
			},strut); /* Check old object */
				return;
			}
			delete strut;
			callNextCallback();
			return;
		}, s);
		return; /* Get logical drive of the old object */
	}

#endif /* !_FS_READONLY */
#endif /* _NB_FS_MINIMIZE == 0 */
#endif /* _NB_FS_MINIMIZE <= 1 */
#endif /* _NB_FS_MINIMIZE <= 2 */

#if _NB_USE_CHMOD && !_NB_FS_READONLY
#error
/*-----------------------------------------------------------------------*/
/* Change Attribute                                                      */
/*-----------------------------------------------------------------------*/

FRESULT f_chmod(
	const TCHAR *path, /* Pointer to the file path */
	BYTE attr,		   /* Attribute bits */
	BYTE mask		   /* Attribute mask to change */
	,
	void (*callback)(void *))
{
	addCallback(callback, data);
	FRESULT res;
	DIR dj;
	FATFS *fs;
	DEF_NAMBUF

	res = find_volume(&path, &fs, FA_WRITE); /* Get logical drive */
	dj.obj.fs = fs;
	if (res == FR_OK)
	{
		INIT_NAMBUF(fs);
		res = follow_path(&dj, path); /* Follow the file path */
		if (res == FR_OK && (dj.fn[NSFLAG] & (NS_DOT | NS_NONAME)))
			res = FR_INVALID_NAME; /* Check object validity */
		if (res == FR_OK)
		{
			mask &= AM_READ_ONLY | AM_HIDDEN | AM_SYSTEM | AM_ARCHIVE; /* Valid attribute mask */
#if _NB_FS_EXFAT
			if (fs->fs_type == FS_EXFAT)
			{
				fs->dirBuffer[XDIR_Attr] = (attr & mask) | (fs->dirBuffer[XDIR_Attr] & (BYTE)~mask); /* Apply attribute change */
				res = store_xdir(&dj);
			}
			else
#endif
			{
				dj.dirPtr[DIR_Attr] = (attr & mask) | (dj.dirPtr[DIR_Attr] & (BYTE)~mask); /* Apply attribute change */
				fs->wflag = 1;
			}
			if (res == FR_OK)
			{
				res = sync_fs(fs);
			}
		}
		FREE_NAMBUF();
	}

	LEAVE_FF(fs, res);
}

/*-----------------------------------------------------------------------*/
/* Change Timestamp                                                      */
/*-----------------------------------------------------------------------*/

FRESULT f_utime(
	const TCHAR *path, /* Pointer to the file/directory name */
	const FILINFO *fno /* Pointer to the time stamp to be set */
	,
	void (*callback)(void *))
{
	addCallback(callback, data);
	FRESULT res;
	DIR dj;
	FATFS *fs;
	DEF_NAMBUF

	res = find_volume(&path, &fs, FA_WRITE); /* Get logical drive */
	dj.obj.fs = fs;
	if (res == FR_OK)
	{
		INIT_NAMBUF(fs);
		res = follow_path(&dj, path); /* Follow the file path */
		if (res == FR_OK && (dj.fn[NSFLAG] & (NS_DOT | NS_NONAME)))
			res = FR_INVALID_NAME; /* Check object validity */
		if (res == FR_OK)
		{
#if _NB_FS_EXFAT
			if (fs->fs_type == FS_EXFAT)
			{
				st_dword(fs->dirBuffer + XDIR_ModTime, (DWORD)fno->fdate << 16 | fno->ftime);
				res = store_xdir(&dj);
			}
			else
#endif
			{
				st_dword(dj.dirPtr + DIR_ModTime, (DWORD)fno->fdate << 16 | fno->ftime);
				fs->wflag = 1;
			}
			if (res == FR_OK)
			{
				res = sync_fs(fs);
			}
		}
		FREE_NAMBUF();
	}

	LEAVE_FF(fs, res);
}

#endif /* _USE_CHMOD && !_FS_READONLY */

#if _NB_USE_LABEL
#error
/*-----------------------------------------------------------------------*/
/* Get Volume Label                                                      */
/*-----------------------------------------------------------------------*/

FRESULT f_getlabel(
	const TCHAR *path, /* Path name of the logical drive number */
	TCHAR *label,	   /* Pointer to a buffer to return the volume label */
	DWORD *vsn		   /* Pointer to a variable to return the volume serial number */
	,
	void (*callback)(void *))
{
	addCallback(callback, data);
	FRESULT res;
	DIR dj;
	FATFS *fs;
	UINT si, di;
#if _NB_LFN_UNICODE || _NB_FS_EXFAT
	WCHAR w;
#endif

	/* Get logical drive */
	res = find_volume(&path, &fs, 0);

	/* Get volume label */
	if (res == FR_OK && label)
	{
		dj.obj.fs = fs;
		dj.obj.startCluster = 0; /* Open root directory */
		res = dir_sdi(&dj, 0);
		if (res == FR_OK)
		{
			res = dir_read(&dj, 1); /* Find a volume label entry */
			if (res == FR_OK)
			{
#if _NB_FS_EXFAT
				if (fs->fs_type == FS_EXFAT)
				{
					for (si = di = 0; si < dj.dirPtr[XDIR_NumLabel]; si++)
					{ /* Extract volume label from 83 entry */
						w = ld_word(dj.dirPtr + XDIR_Label + si * 2);
#if _NB_LFN_UNICODE
						label[di++] = w;
#else
						w = ff_convert(w, 0); /* Unicode -> OEM */
						if (w == 0)
							w = '?'; /* Replace wrong character */
						if (_DF1S && w >= 0x100)
							label[di++] = (char)(w >> 8);
						label[di++] = (char)w;
#endif
					}
					label[di] = 0;
				}
				else
#endif
				{
					si = di = 0; /* Extract volume label from AM_VOL entry with code comversion */
					do
					{
#if _NB_LFN_UNICODE
						w = (si < 11) ? dj.dirPtr[si++] : ' ';
						if (IsDBCS1(w) && si < 11 && IsDBCS2(dj.dirPtr[si]))
						{
							w = w << 8 | dj.dirPtr[si++];
						}
						label[di++] = ff_convert(w, 1); /* OEM -> Unicode */
#else
						label[di++] = dj.dirPtr[si++];
#endif
					} while (di < 11);
					do
					{ /* Truncate trailing spaces */
						label[di] = 0;
						if (di == 0)
							break;
					} while (label[--di] == ' ');
				}
			}
		}
		if (res == FR_NO_FILE)
		{ /* No label entry and return nul string */
			label[0] = 0;
			res = FR_OK;
		}
	}

	/* Get volume serial number */
	if (res == FR_OK && vsn)
	{
		res = move_window(fs, fs->volBaseSector);
		if (res == FR_OK)
		{
			switch (fs->fs_type)
			{
			case FS_EXFAT:
				di = BPB_VolIDEx;
				break;

			case FS_FAT32:
				di = BS_VolID32;
				break;

			default:
				di = BS_VolID;
			}
			*vsn = ld_dword(fs->win + di);
		}
	}

	LEAVE_FF(fs, res);
}

#if !_NB_FS_READONLY
#error
/*-----------------------------------------------------------------------*/
/* Set Volume Label                                                      */
/*-----------------------------------------------------------------------*/

FRESULT f_setlabel(
	const TCHAR *label /* Pointer to the volume label to set */
	,
	void (*callback)(void *))
{
	addCallback(callback, data);
	FRESULT res;
	DIR dj;
	FATFS *fs;
	BYTE dirvn[22];
	UINT i, j, slen;
	WCHAR w;
	static const char badchr[] = "\"*+,.:;<=>\?[]|\x7F";

	/* Get logical drive */
	res = find_volume(&label, &fs, FA_WRITE);
	if (res != FR_OK)
		LEAVE_FF(fs, res);
	dj.obj.fs = fs;

	/* Get length of given volume label */
	for (slen = 0; (UINT)label[slen] >= ' '; slen++)
		; /* Get name length */

#if _NB_FS_EXFAT
	if (fs->fs_type == FS_EXFAT)
	{ /* On the exFAT volume */
		for (i = j = 0; i < slen;)
		{ /* Create volume label in directory form */
			w = label[i++];
#if !_NB_LFN_UNICODE
			if (IsDBCS1(w))
			{
				w = (i < slen && IsDBCS2(label[i])) ? w << 8 | (BYTE)label[i++] : 0;
			}
			w = ff_convert(w, 1);
#endif
			if (w == 0 || chk_chr(badchr, w) || j == 22)
			{ /* Check validity check validity of the volume label */
				LEAVE_FF(fs, FR_INVALID_NAME);
			}
			st_word(dirvn + j, w);
			j += 2;
		}
		slen = j;
	}
	else
#endif
	{ /* On the FAT12/16/32 volume */
		for (; slen && label[slen - 1] == ' '; slen--)
			; /* Remove trailing spaces */
		if (slen)
		{ /* Is there a volume label to be set? */
			dirvn[0] = 0;
			i = j = 0; /* Create volume label in directory form */
			do
			{
#if _NB_LFN_UNICODE
				w = ff_convert(ff_wtoupper(label[i++]), 0);
#else
				w = (BYTE)label[i++];
				if (IsDBCS1(w))
				{
					w = (j < 10 && i < slen && IsDBCS2(label[i])) ? w << 8 | (BYTE)label[i++] : 0;
				}
#if _NB_USE_LFN != 0
				w = ff_convert(ff_wtoupper(ff_convert(w, 1)), 0);
#else
				if (IsLower(w))
					w -= 0x20; /* To upper ASCII characters */
#ifdef _EXCVT
				if (w >= 0x80)
					w = ExCvt[w - 0x80]; /* To upper extended characters (SBCS cfg) */
#else
				if (!_DF1S && w >= 0x80)
					w = 0; /* Reject extended characters (ASCII cfg) */
#endif
#endif
#endif
				if (w == 0 || chk_chr(badchr, w) || j >= (UINT)((w >= 0x100) ? 10 : 11))
				{ /* Reject invalid characters for volume label */
					LEAVE_FF(fs, FR_INVALID_NAME);
				}
				if (w >= 0x100)
					dirvn[j++] = (BYTE)(w >> 8);
				dirvn[j++] = (BYTE)w;
			} while (i < slen);
			while (j < 11)
				dirvn[j++] = ' '; /* Fill remaining name field */
			if (dirvn[0] == DDEM)
				LEAVE_FF(fs, FR_INVALID_NAME); /* Reject illegal name (heading DDEM) */
		}
	}

	/* Set volume label */
	dj.obj.startCluster = 0; /* Open root directory */
	res = dir_sdi(&dj, 0);
	if (res == FR_OK)
	{
		res = dir_read(&dj, 1); /* Get volume label entry */
		if (res == FR_OK)
		{
			if (_NB_FS_EXFAT && fs->fs_type == FS_EXFAT)
			{
				dj.dirPtr[XDIR_NumLabel] = (BYTE)(slen / 2); /* Change the volume label */
				mem_cpy(dj.dirPtr + XDIR_Label, dirvn, slen);
			}
			else
			{
				if (slen)
				{
					mem_cpy(dj.dirPtr, dirvn, 11); /* Change the volume label */
				}
				else
				{
					dj.dirPtr[DIR_Name] = DDEM; /* Remove the volume label */
				}
			}
			fs->wflag = 1;
			res = sync_fs(fs);
		}
		else
		{ /* No volume label entry is found or error */
			if (res == FR_NO_FILE)
			{
				res = FR_OK;
				if (slen)
				{							 /* Create a volume label entry */
					res = dir_alloc(&dj, 1); /* Allocate an entry */
					if (res == FR_OK)
					{
						mem_set(dj.dirPtr, 0, SZDIRE); /* Clear the entry */
						if (_NB_FS_EXFAT && fs->fs_type == FS_EXFAT)
						{
							dj.dirPtr[XDIR_Type] = 0x83; /* Create 83 entry */
							dj.dirPtr[XDIR_NumLabel] = (BYTE)(slen / 2);
							mem_cpy(dj.dirPtr + XDIR_Label, dirvn, slen);
						}
						else
						{
							dj.dirPtr[DIR_Attr] = AM_VOL; /* Create volume label entry */
							mem_cpy(dj.dirPtr, dirvn, 11);
						}
						fs->wflag = 1;
						res = sync_fs(fs);
					}
				}
			}
		}
	}

	LEAVE_FF(fs, res);
}

#endif /* !_FS_READONLY */
#endif /* _NB_USE_LABEL */

#if _NB_USE_EXPAND && !_NB_FS_READONLY
#error
/*-----------------------------------------------------------------------*/
/* Allocate a Contiguous Blocks to the File                              */
/*-----------------------------------------------------------------------*/

FRESULT f_expand(
	FIL *fp,	 /* Pointer to the file object */
	FSIZE_t fsz, /* File size to be expanded to */
	BYTE opt	 /* Operation mode 0:Find and prepare or 1:Find and allocate */
	,
	void (*callback)(void *))
{
	addCallback(callback, data);
	FRESULT res;
	FATFS *fs;
	DWORD n, clst, stcl, scl, ncl, tcl, lclst;

	res = validate(&fp->obj, &fs); /* Check validity of the file object */
	if (res != FR_OK || (res = (FRESULT)fp->err) != FR_OK)
		LEAVE_FF(fs, res);
	if (fsz == 0 || fp->obj.objSize != 0 || !(fp->flag & FA_WRITE))
		LEAVE_FF(fs, FR_DENIED);
#if _NB_FS_EXFAT
	if (fs->fs_type != FS_EXFAT && fsz >= 0x100000000)
		LEAVE_FF(fs, FR_DENIED); /* Check if in size limit */
#endif
	n = (DWORD)fs->clusterSize * SS(fs);				/* Cluster size */
	tcl = (DWORD)(fsz / n) + ((fsz & (n - 1)) ? 1 : 0); /* Number of clusters required */
	stcl = fs->lastCluster;
	lclst = 0;
	if (stcl < 2 || stcl >= fs->numberOfFatEntries)
		stcl = 2;

#if _NB_FS_EXFAT
	if (fs->fs_type == FS_EXFAT)
	{
		scl = find_bitmap(fs, stcl, tcl); /* Find a contiguous cluster block */
		if (scl == 0)
			res = FR_DENIED; /* No contiguous cluster block was found */
		if (scl == 0xFFFFFFFF)
			res = FR_DISK_ERROR;
		if (res == FR_OK)
		{ /* A contiguous free area is found */
			if (opt)
			{										  /* Allocate it now */
				res = change_bitmap(fs, scl, tcl, 1); /* Mark the cluster block 'in use' */
				lclst = scl + tcl - 1;
			}
			else
			{ /* Set it as suggested point for next allocation */
				lclst = scl - 1;
			}
		}
	}
	else
#endif
	{
		scl = clst = stcl;
		ncl = 0;
		for (;;)
		{ /* Find a contiguous cluster block */
			n = get_fat(&fp->obj, clst);
			if (++clst >= fs->numberOfFatEntries)
				clst = 2;
			if (n == 1)
			{
				res = FR_INT_ERROR;
				break;
			}
			if (n == 0xFFFFFFFF)
			{
				res = FR_DISK_ERROR;
				break;
			}
			if (n == 0)
			{ /* Is it a free cluster? */
				if (++ncl == tcl)
					break; /* Break if a contiguous cluster block is found */
			}
			else
			{
				scl = clst;
				ncl = 0; /* Not a free cluster */
			}
			if (clst == stcl)
			{
				res = FR_DENIED;
				break;
			} /* No contiguous cluster? */
		}
		if (res == FR_OK)
		{ /* A contiguous free area is found */
			if (opt)
			{ /* Allocate it now */
				for (clst = scl, n = tcl; n; clst++, n--)
				{ /* Create a cluster chain on the FAT */
					res = put_fat(fs, clst, (n == 1) ? 0xFFFFFFFF : clst + 1);
					if (res != FR_OK)
						break;
					lclst = clst;
				}
			}
			else
			{ /* Set it as suggested point for next allocation */
				lclst = scl - 1;
			}
		}
	}

	if (res == FR_OK)
	{
		fs->lastCluster = lclst; /* Set suggested start cluster to start next */
		if (opt)
		{								/* Is it allocated now? */
			fp->obj.startCluster = scl; /* Update object allocation information */
			fp->obj.objSize = fsz;
			if (_NB_FS_EXFAT)
				fp->obj.stat = 2; /* Set status 'contiguous chain' */
			fp->flag |= FA_MODIFIED;
			if (fs->numberOfFreeClusters <= fs->numberOfFatEntries - 2)
			{ /* Update FSINFO */
				fs->numberOfFreeClusters -= tcl;
				fs->fsi_flag |= 1;
			}
		}
	}

	LEAVE_FF(fs, res);
}

#endif /* _USE_EXPAND && !_FS_READONLY */

#if _NB_USE_FORWARD
#error
/*-----------------------------------------------------------------------*/
/* Forward data to the stream directly                                   */
/*-----------------------------------------------------------------------*/

FRESULT f_forward(
	FIL *fp,						  /* Pointer to the file object */
	UINT (*func)(const BYTE *, UINT), /* Pointer to the streaming function */
	UINT btf,						  /* Number of bytes to forward */
	UINT *bf						  /* Pointer to number of bytes forwarded */
	,
	void (*callback)(void *))
{
	addCallback(callback, data);
	FRESULT res;
	FATFS *fs;
	DWORD clst, sectorNumber;
	FSIZE_t remain;
	UINT rcnt, csect;
	BYTE *dbuf;

	*bf = 0;					   /* Clear transfer byte counter */
	res = validate(&fp->obj, &fs); /* Check validity of the file object */
	if (res != FR_OK || (res = (FRESULT)fp->err) != FR_OK)
		LEAVE_FF(fs, res);
	if (!(fp->flag & FA_READ))
		LEAVE_FF(fs, FR_DENIED); /* Check access mode */

	remain = fp->obj.objSize - fp->fileReadWritePtr;
	if (btf > remain)
		btf = (UINT)remain; /* Truncate btf by remaining bytes */

	for (; btf && (*func)(0, 0); /* Repeat until all data transferred or stream goes busy */
		 fp->fileReadWritePtr += rcnt, *bf += rcnt, btf -= rcnt)
	{
		csect = (UINT)(fp->fileReadWritePtr / SS(fs) & (fs->clusterSize - 1)); /* Sector offset in the cluster */
		if (fp->fileReadWritePtr % SS(fs) == 0)
		{ /* On the sector boundary? */
			if (csect == 0)
			{										 /* On the cluster boundary? */
				clst = (fp->fileReadWritePtr == 0) ? /* On the top of the file? */
						   fp->obj.startCluster
												   : get_fat(&fp->obj, fp->currentFilePtrCluster);
				if (clst <= 1)
					ABORT(fs, FR_INT_ERROR);
				if (clst == 0xFFFFFFFF)
					ABORT(fs, FR_DISK_ERROR);
				fp->currentFilePtrCluster = clst; /* Update current cluster */
			}
		}
		sectorNumber = clust2sect(fs, fp->currentFilePtrCluster); /* Get current data sector */
		if (!sectorNumber)
			ABORT(fs, FR_INT_ERROR);
		sectorNumber += csect;
#if _NB_FS_TINY
		if (move_window(fs, sectorNumber) != FR_OK)
			ABORT(fs, FR_DISK_ERROR); /* Move sector window to the file data */
		dbuf = fs->win;
#else
		if (fp->sectorNumber != sectorNumber)
		{ /* Fill sector cache with file data */
#if !_NB_FS_READONLY
			if (fp->flag & FA_DIRTY)
			{ /* Write-back dirty sector cache */
				voidPtr = strut;
				if (disk_write(fs->driveNumber, fp->buf, fp->sectorNumber, 1) != RES_OK)
					ABORT(fs, FR_DISK_ERROR);
				fp->flag &= (BYTE)~FA_DIRTY;
			}
#endif
			voidPtr = strut;
			if (disk_read(fs->driveNumber, fp->buf, sectorNumber, 1) != RES_OK)
				ABORT(fs, FR_DISK_ERROR);
		}
		dbuf = fp->buf;
#endif
		fp->sectorNumber = sectorNumber;
		rcnt = SS(fs) - (UINT)fp->fileReadWritePtr % SS(fs); /* Number of bytes left in the sector */
		if (rcnt > btf)
			rcnt = btf;														/* Clip it by btr if needed */
		rcnt = (*func)(dbuf + ((UINT)fp->fileReadWritePtr % SS(fs)), rcnt); /* Forward the file data */
		if (!rcnt)
			ABORT(fs, FR_INT_ERROR);
	}

	LEAVE_FF(fs, FR_OK);
}
#endif /* _USE_FORWARD */

#if _NB_USE_MKFS && !_NB_FS_READONLY
	/*-----------------------------------------------------------------------*/
	/* Create an FAT/exFAT volume                                            */
	/*-----------------------------------------------------------------------*/

	static const WORD cst[7] = { 1, 4, 16, 64, 256, 512, 0 }; /* Cluster size boundary for FAT12/16 volume (4Ks unit) */
	static const WORD cst32[7] = { 1, 2, 4, 8, 16, 32, 0 }; /* Cluster size boundary for FAT32 volume (128Ks unit) */

	struct f_mkfs_strut {
		const TCHAR *path; /* Logical drive number */
		BYTE opt; /* Format option */
		DWORD au; /* Size of allocation unit (cluster) [byte] */
		void *work; /* Pointer to working buffer */
		UINT len; /* Size of working buffer */
		const UINT n_fats = 1; /* Number of FATs for FAT12/16/32 volume (1 or 2) */
		const UINT n_rootdir = 512; /* Number of root directory entries for FAT12/16 volume */
		BYTE fmt, sys, *buf, *pte, pdrv, part;
		WORD ss;
		DWORD szb_buf, sz_buf, sz_blk, n_clst, pau, sect, nsect, n;
		DWORD b_vol, b_fat, b_data; /* Base LBA for volume, fat, data */
		DWORD sz_vol, sz_rsv, sz_fat, sz_dir; /* Size for volume, fat, dir, data */
		UINT i;
		int vol;
		DSTATUS stat;
#if _NB_USE_TRIM || _NB_FS_EXFAT
		DWORD tbl[3];
#endif
#if _NB_FS_EXFAT
		DWORD szb_bit, szb_case, sum, nb, cl;
		WCHAR ch, si;
		UINT j, st;
		BYTE b;
#endif
	}
	;

	void f_mkfs_end(f_mkfs_strut *strut) {
		if (disk_ioctl(strut->pdrv, CTRL_SYNC, 0) != RES_OK) {
			fResult = FR_DISK_ERROR;
			delete strut;
			callNextCallback();
			return;
		}
		fResult = FR_OK;
		delete strut;
		callNextCallback();
		return;
	}

	void f_mkfs_b(f_mkfs_strut *strut) {
		/* Determine system ID in the partition table */
		if (_NB_FS_EXFAT && strut->fmt == FS_EXFAT) {
			strut->sys = 0x07; /* HPFS/NTFS/exFAT */
		} else {
			if (strut->fmt == FS_FAT32) {
				strut->sys = 0x0C; /* FAT32X */
			} else {
				if (strut->pau >= 0x10000) {
					strut->sys = 0x06; /* FAT12/16 (>=64KS) */
				} else {
					strut->sys = (strut->fmt == FS_FAT16) ? 0x04 : 0x01; /* FAT16 (<64KS) : FAT12 (<64KS) */
				}
			}
		}

		/* Update partition information */
		if (_NB_MULTI_PARTITION && strut->part != 0) { /* Created in the existing partition */
			/* Update system ID in the partition table */
			voidPtr = strut;
			disk_read(strut->pdrv, strut->buf, 0, 1, [](DRESULT dRes) {
				f_mkfs_strut *strut = (f_mkfs_strut*) voidPtr;
				if (dRes != RES_OK) {
					fResult = FR_DISK_ERROR;
					delete strut;
					callNextCallback();
					return;
				} /* Read the MBR */
				strut->buf[MBR_Table + (strut->part - 1) * SZ_PTE + PTE_System] = strut->sys; /* Set system ID */
				voidPtr = strut;
				disk_write(strut->pdrv, strut->buf, 0, 1, [](DRESULT dRes) {
					f_mkfs_strut *strut = (f_mkfs_strut*) voidPtr;
					if (dRes != RES_OK) {
						fResult = FR_DISK_ERROR;
						delete strut;
						callNextCallback();
						return;
					} /* Write it back to the MBR */
					f_mkfs_end(strut);
					return;
				});
				return;
			});
			return;
		} else { /* Created as a new single partition */
			if (!(strut->opt & FM_SFD)) { /* Create partition table if in FDISK format */
				mem_set(strut->buf, 0, strut->ss);
				st_word(strut->buf + BS_55AA, 0xAA55); /* MBR signature */
				strut->pte = strut->buf + MBR_Table; /* Create partition table for single partition in the drive */
				strut->pte[PTE_Boot] = 0; /* Boot indicator */
				strut->pte[PTE_StHead] = 1; /* Start head */
				strut->pte[PTE_StSec] = 1; /* Start sector */
				strut->pte[PTE_StCyl] = 0; /* Start cylinder */
				strut->pte[PTE_System] = strut->sys; /* System type */
				strut->n = (strut->b_vol + strut->pau) / (63 * 255); /* (End CHS may be invalid) */
				strut->pte[PTE_EdHead] = 254; /* End head */
				strut->pte[PTE_EdSec] = (BYTE) (strut->n >> 2 | 63); /* End sector */
				strut->pte[PTE_EdCyl] = (BYTE) strut->n; /* End cylinder */
				st_dword(strut->pte + PTE_StLba, strut->b_vol); /* Start offset in LBA */
				st_dword(strut->pte + PTE_SizLba, strut->pau); /* Size in sectors */
				voidPtr = strut;
				disk_write(strut->pdrv, strut->buf, 0, 1, [](DRESULT dRes) {
					f_mkfs_strut *strut = (f_mkfs_strut*) voidPtr;
					if (dRes != RES_OK) {
						fResult = FR_DISK_ERROR;
						delete strut;
						callNextCallback();
						return;
					} /* Write it to the MBR */
					f_mkfs_end(strut);
					return;
				});
				return;
			}
		}
	}

	void f_mkfs_a_loop(f_mkfs_strut *strut) {
		strut->n = (strut->nsect > strut->sz_buf) ? strut->sz_buf : strut->nsect;
		voidPtr = strut;
		disk_write(strut->pdrv, strut->buf, strut->sect, (UINT) strut->n, [](DRESULT dRes) {
			f_mkfs_strut *strut = (f_mkfs_strut*) voidPtr;
			if (dRes != RES_OK) {
				fResult = FR_DISK_ERROR;
				delete strut;
				callNextCallback();
				return;
			}
			strut->sect += strut->n;
			strut->nsect -= strut->n;
			if (strut->nsect) {
				f_mkfs_a_loop(strut);
				return;
			} else {
				f_mkfs_b(strut);
				return;
			}
		});
		return;
	}
	void f_mkfs_d_loope(f_mkfs_strut *strut);
	void f_mkfs_g_loop(f_mkfs_strut *strut);

	void f_mkfs_a_a_loop_b(f_mkfs_strut *strut);
	void f_mkfs_a_a_loop(f_mkfs_strut *strut) {
		/* Fill FAT sectors */
		strut->n = (strut->nsect > strut->sz_buf) ? strut->sz_buf : strut->nsect;
		voidPtr = strut;
		disk_write(strut->pdrv, strut->buf, strut->sect, (UINT) strut->n, [](DRESULT dRes) {
			f_mkfs_strut *strut = (f_mkfs_strut*) voidPtr;
			if (dRes != RES_OK) {
				fResult = FR_DISK_ERROR;
				delete strut;
				callNextCallback();
				return;
			}
			mem_set(strut->buf, 0, strut->ss);
			strut->sect += strut->n;
			strut->nsect -= strut->n;
			if (strut->nsect) {
				f_mkfs_a_a_loop(strut);
				return;
			} else {
				strut->i++;
				f_mkfs_a_a_loop_b(strut);
				return;
			}
		});
		return;
	}

	void f_mkfs_a_a_loop_b(f_mkfs_strut *strut) {
		if (strut->i < strut->n_fats) { /* Initialize FATs each */
			if (strut->fmt == FS_FAT32) {
				st_dword(strut->buf + 0, 0xFFFFFFF8); /* Entry 0 */
				st_dword(strut->buf + 4, 0xFFFFFFFF); /* Entry 1 */
				st_dword(strut->buf + 8, 0x0FFFFFFF); /* Entry 2 (root directory) */
			} else {
				st_dword(strut->buf + 0, (strut->fmt == FS_FAT12) ? 0xFFFFF8 : 0xFFFFFFF8); /* Entry 0 and 1 */
			}
			strut->nsect = strut->sz_fat; /* Number of FAT sectors */
			f_mkfs_a_a_loop(strut);
			return;
		} else {
			/* Initialize root directory (fill with zero) */
			strut->nsect = (strut->fmt == FS_FAT32) ? strut->pau : strut->sz_dir; /* Number of root directory sectors */
			f_mkfs_a_loop(strut);
			return;
		}
	}

	void f_mkfs_a_a(f_mkfs_strut *strut) {
		/* Initialize FAT area */
		mem_set(strut->buf, 0, (UINT) strut->szb_buf);
		strut->sect = strut->b_fat; /* FAT start sector */
		strut->i = 0;
		f_mkfs_a_a_loop_b(strut);
		return;
	}

	void f_mkfs_d_loope_c_loop(f_mkfs_strut *strut) {
		/* Sum record (+11) */
		for (strut->i = 0; strut->i < strut->ss; strut->i += 4) {
			st_dword(strut->buf + strut->i, strut->sum); /* Fill with checksum value */
		}
		voidPtr = strut;
		disk_write(strut->pdrv, strut->buf, strut->sect++, 1, [](DRESULT dRes) {
			f_mkfs_strut *strut = (f_mkfs_strut*) voidPtr;
			if (dRes != RES_OK) {
				fResult = FR_DISK_ERROR;
				delete strut;
				callNextCallback();
				return;
			}
			strut->n++;
			f_mkfs_d_loope(strut);
			return;
		});
		return;
	}

	void f_mkfs_d_loope_b_loop(f_mkfs_strut *strut) {
		/* OEM/Reserved record (+9..+10) */
		if (strut->j < 11) {
			for (strut->i = 0; strut->i < strut->ss; strut->sum = xsum32(strut->buf[strut->i++], strut->sum))
				; /* VBR checksum */
			voidPtr = strut;
			disk_write(strut->pdrv, strut->buf, strut->sect++, 1, [](DRESULT dRes) {
				f_mkfs_strut *strut = (f_mkfs_strut*) voidPtr;
				if (dRes != RES_OK) {
					fResult = FR_DISK_ERROR;
					delete strut;
					callNextCallback();
					return;
				}
				strut->j++;
				f_mkfs_d_loope_b_loop(strut);
				return;
			});
			return;
		} else {
			f_mkfs_d_loope_c_loop(strut);
			return;
		}
	}

	void f_mkfs_d_loope_a_loop(f_mkfs_strut *strut) {
		if (strut->j < 9) {
			for (strut->i = 0; strut->i < strut->ss; strut->sum = xsum32(strut->buf[strut->i++], strut->sum))
				; /* VBR checksum */
			voidPtr = strut;
			disk_write(strut->pdrv, strut->buf, strut->sect++, 1, [](DRESULT dRes) {
				f_mkfs_strut *strut = (f_mkfs_strut*) voidPtr;
				if (dRes != RES_OK) {
					fResult = FR_DISK_ERROR;
					delete strut;
					callNextCallback();
					return;
				}
				strut->j++;
				f_mkfs_d_loope_a_loop(strut);
				return;
			});
			return;
		} else {
			mem_set(strut->buf, 0, strut->ss);
			f_mkfs_d_loope_b_loop(strut);
			return;
		}
	}

	void f_mkfs_d_loope(f_mkfs_strut *strut) {

		/* Create two set of the exFAT VBR blocks */
		if (strut->n < 2) {
			/* Main record (+0) */
			mem_set(strut->buf, 0, strut->ss);
			mem_cpy(strut->buf + BS_JmpBoot, "\xEB\x76\x90"
					"EXFAT   ", 11); /* Boot jump code (x86), OEM name */
			st_dword(strut->buf + BPB_VolOfsEx, strut->b_vol); /* Volume offset in the physical drive [sector] */
			st_dword(strut->buf + BPB_TotSecEx, strut->sz_vol); /* Volume size [sector] */
			st_dword(strut->buf + BPB_FatOfsEx, strut->b_fat - strut->b_vol); /* FAT offset [sector] */
			st_dword(strut->buf + BPB_FatSzEx, strut->sz_fat); /* FAT size [sector] */
			st_dword(strut->buf + BPB_DataOfsEx, strut->b_data - strut->b_vol); /* Data offset [sector] */
			st_dword(strut->buf + BPB_NumClusEx, strut->n_clst); /* Number of clusters */
			st_dword(strut->buf + BPB_RootClusEx, 2 + strut->tbl[0] + strut->tbl[1]); /* Root dirPtr cluster # */
			st_dword(strut->buf + BPB_VolIDEx, GET_FATTIME()); /* VSN */
			st_word(strut->buf + BPB_FSVerEx, 0x100); /* File system version (1.00) */
			for (strut->buf[BPB_BytsPerSecEx] = 0, strut->i = strut->ss; strut->i >>= 1; strut->buf[BPB_BytsPerSecEx]++)
				; /* Log2 of sector size [byte] */
			for (strut->buf[BPB_SecPerClusEx] = 0, strut->i = strut->au; strut->i >>= 1; strut->buf[BPB_SecPerClusEx]++)
				; /* Log2 of cluster size [sector] */
			strut->buf[BPB_NumFATsEx] = 1; /* Number of FATs */
			strut->buf[BPB_DrvNumEx] = 0x80; /* Drive number (for int13) */
			st_word(strut->buf + BS_BootCodeEx, 0xFEEB); /* Boot code (x86) */
			st_word(strut->buf + BS_55AA, 0xAA55); /* Signature (placed here regardless of sector size) */
			for (strut->i = strut->sum = 0; strut->i < strut->ss; strut->i++) { /* VBR checksum */
				if (strut->i != BPB_VolFlagEx && strut->i != BPB_VolFlagEx + 1 && strut->i != BPB_PercInUseEx) {
					strut->sum = xsum32(strut->buf[strut->i], strut->sum);
				}
			}
			voidPtr = strut;
			disk_write(strut->pdrv, strut->buf, strut->sect++, 1, [](DRESULT dRes) {
				f_mkfs_strut *strut = (f_mkfs_strut*) voidPtr;
				if (dRes != RES_OK) {
					fResult = FR_DISK_ERROR;
					delete strut;
					callNextCallback();
					return;
				}
				/* Extended bootstrap record (+1..+8) */
				mem_set(strut->buf, 0, strut->ss);
				st_word(strut->buf + strut->ss - 2, 0xAA55); /* Signature (placed at end of sector) */
				strut->j = 1;
				f_mkfs_d_loope_a_loop(strut);
				return;
			});
			return;
		} else {
			f_mkfs_b(strut);
			return;
		}
	}

	void f_mkfs_c_loop(f_mkfs_strut *strut) { /* Fill root directory sectors */
		strut->n = (strut->nsect > strut->sz_buf) ? strut->sz_buf : strut->nsect;
		voidPtr = strut;
		disk_write(strut->pdrv, strut->buf, strut->sect, strut->n, [](DRESULT dRes) {
			f_mkfs_strut *strut = (f_mkfs_strut*) voidPtr;
			if (dRes != RES_OK) {
				fResult = FR_DISK_ERROR;
				delete strut;
				callNextCallback();
				return;
			}
			mem_set(strut->buf, 0, strut->ss);
			strut->sect += strut->n;
			strut->nsect -= strut->n;
			if (strut->nsect) {
				f_mkfs_c_loop(strut);
				return;
			} else {
				strut->sect = strut->b_vol;
				strut->n = 0;
				f_mkfs_d_loope(strut);
				return;
			}
		});
		return;
	}

	void f_mkfs_e_loop(f_mkfs_strut *strut) {
		mem_set(strut->buf, 0, strut->szb_buf);
		strut->i = 0; /* Clear work area and reset write index */
		if (strut->cl == 0) { /* Set entry 0 and 1 */
			st_dword(strut->buf + strut->i, 0xFFFFFFF8);
			strut->i += 4;
			strut->cl++;
			st_dword(strut->buf + strut->i, 0xFFFFFFFF);
			strut->i += 4;
			strut->cl++;
		}
		do { /* Create chains of bitmap, up-case and root dirPtr */
			while (strut->nb && strut->i < strut->szb_buf) { /* Create a chain */
				st_dword(strut->buf + strut->i, (strut->nb > 1) ? strut->cl + 1 : 0xFFFFFFFF);
				strut->i += 4;
				strut->cl++;
				strut->nb--;
			}
			if (!strut->nb && strut->j < 3) {
				strut->nb = strut->tbl[strut->j++];
			} /* Next chain */
		} while (strut->nb && strut->i < strut->szb_buf);
		strut->n = (strut->nsect > strut->sz_buf) ? strut->sz_buf : strut->nsect; /* Write the buffered data */
		voidPtr = strut;
		disk_write(strut->pdrv, strut->buf, strut->sect, strut->n, [](DRESULT dRes) {
			f_mkfs_strut *strut = (f_mkfs_strut*) voidPtr;
			if (dRes != RES_OK) {
				fResult = FR_DISK_ERROR;
				delete strut;
				callNextCallback();
				return;
			}
			strut->sect += strut->n;
			strut->nsect -= strut->n;
			if (strut->nsect) {
				f_mkfs_e_loop(strut);
				return;
			} else {
				/* Initialize the root directory */
				mem_set(strut->buf, 0, strut->szb_buf);
				strut->buf[SZDIRE * 0 + 0] = 0x83; /* 83 entry (volume label) */
				strut->buf[SZDIRE * 1 + 0] = 0x81; /* 81 entry (allocation bitmap) */
				st_dword(strut->buf + SZDIRE * 1 + 20, 2);
				st_dword(strut->buf + SZDIRE * 1 + 24, strut->szb_bit);
				strut->buf[SZDIRE * 2 + 0] = 0x82; /* 82 entry (up-case table) */
				st_dword(strut->buf + SZDIRE * 2 + 4, strut->sum);
				st_dword(strut->buf + SZDIRE * 2 + 20, 2 + strut->tbl[0]);
				st_dword(strut->buf + SZDIRE * 2 + 24, strut->szb_case);
				strut->sect = strut->b_data + strut->au * (strut->tbl[0] + strut->tbl[1]);
				strut->nsect = strut->au; /* Start of the root directory and number of sectors */
				f_mkfs_c_loop(strut);
				return;
			}
		});
		return;
	}

	void f_mkfs_f_loop(f_mkfs_strut *strut) {
		mem_set(strut->buf, 0, strut->szb_buf);
		for (strut->i = 0; strut->nb >= 8 && strut->i < strut->szb_buf; strut->buf[strut->i++] = 0xFF, strut->nb -= 8)
			;
		for (strut->b = 1; strut->nb && strut->i < strut->szb_buf; strut->buf[strut->i] |= strut->b, strut->b <<= 1, strut->nb--)
			;
		strut->n = (strut->nsect > strut->sz_buf) ? strut->sz_buf : strut->nsect; /* Write the buffered data */
		voidPtr = strut;
		disk_write(strut->pdrv, strut->buf, strut->sect, strut->n, [](DRESULT dRes) {
			f_mkfs_strut *strut = (f_mkfs_strut*) voidPtr;
			if (dRes != RES_OK) {
				fResult = FR_DISK_ERROR;
				delete strut;
				callNextCallback();
				return;
			}
			strut->sect += strut->n;
			strut->nsect -= strut->n;
			if (strut->nsect) {
				f_mkfs_f_loop(strut);
				return;
			} else {
				/* Initialize the FAT */
				strut->sect = strut->b_fat;
				strut->nsect = strut->sz_fat; /* Start of FAT and number of FAT sectors */
				strut->j = strut->nb = strut->cl = 0;
				f_mkfs_e_loop(strut);
				return;
			}
		});
		return;
	}

	void f_mkfs_g_loop_cont(f_mkfs_strut *strut) {
		if (strut->si) {
			f_mkfs_g_loop(strut);
			return;
		} else {
			strut->tbl[1] = (strut->szb_case + strut->au * strut->ss - 1) / (strut->au * strut->ss); /* Number of up-case table clusters */
			strut->tbl[2] = 1; /* Number of root dirPtr clusters */

			/* Initialize the allocation bitmap */
			strut->sect = strut->b_data;
			strut->nsect = (strut->szb_bit + strut->ss - 1) / strut->ss; /* Start of bitmap and number of sectors */
			strut->nb = strut->tbl[0] + strut->tbl[1] + strut->tbl[2]; /* Number of clusters in-use by system */
			f_mkfs_f_loop(strut);
			return;
		}
	}

	void f_mkfs_g_loop(f_mkfs_strut *strut) {
		switch (strut->st) {
			case 0:
				strut->ch = ff_wtoupper(strut->si); /* Get an up-case char */
				if (strut->ch != strut->si) {
					strut->si++;
					break; /* Store the up-case char if exist */
				}
				for (strut->j = 1; (WCHAR) (strut->si + strut->j) && (WCHAR) (strut->si + strut->j) == ff_wtoupper((WCHAR) (strut->si + strut->j));
						strut->j++)
					; /* Get run length of no-case block */
				if (strut->j >= 128) {
					strut->ch = 0xFFFF;
					strut->st = 2;
					break; /* Compress the no-case block if run is >= 128 */
				}
				strut->st = 1; /* Do not compress short run */
				/* go to next case */
			case 1:
				strut->ch = strut->si++; /* Fill the short run */
				if (--strut->j == 0) {
					strut->st = 0;
				}
				break;

			default:
				strut->ch = (WCHAR) strut->j;
				strut->si += strut->j; /* Number of chars to skip */
				strut->st = 0;
		}
		strut->sum = xsum32(strut->buf[strut->i + 0] = (BYTE) strut->ch, strut->sum); /* Put it into the write buffer */
		strut->sum = xsum32(strut->buf[strut->i + 1] = (BYTE) (strut->ch >> 8), strut->sum);
		strut->i += 2;
		strut->szb_case += 2;
		if (!strut->si || strut->i == strut->szb_buf) { /* Write buffered data when buffer full or end of process */
			strut->n = (strut->i + strut->ss - 1) / strut->ss;
			voidPtr = strut;
			disk_write(strut->pdrv, strut->buf, strut->sect, strut->n, [](DRESULT dRes) {
				f_mkfs_strut *strut = (f_mkfs_strut*) voidPtr;
				if (dRes != RES_OK) {
					fResult = FR_DISK_ERROR;
					delete strut;
					callNextCallback();
					return;
				}
				strut->sect += strut->n;
				strut->i = 0;
				f_mkfs_g_loop_cont(strut);
				return;
			});
			return;
		}
		f_mkfs_g_loop_cont(strut);
		return;
	}

	void f_mkfs_a(f_mkfs_strut *strut) {
		if (strut->sz_vol < 128) {
			fResult = FR_MKFS_ABORTED;
			delete strut;
			callNextCallback();
			return;
		} /* Check if volume size is >=128s */

		/* Pre-determine the FAT type */
		do {
			if (_NB_FS_EXFAT && (strut->opt & FM_EXFAT)) { /* exFAT possible? */
				if ((strut->opt & FM_ANY) == FM_EXFAT || strut->sz_vol >= 0x4000000 || strut->au > 128) { /* exFAT only, vol >= 64Ms or au > 128s ? */
					strut->fmt = FS_EXFAT;
					break;
				}
			}
			if (strut->au > 128) {
				fResult = FR_INVALID_PARAMETER;
				delete strut;
				callNextCallback();
				return;
			} /* Too large au for FAT/FAT32 */
			if (strut->opt & FM_FAT32) { /* FAT32 possible? */
				if ((strut->opt & FM_ANY) == FM_FAT32 || !(strut->opt & FM_FAT)) { /* FAT32 only or no-FAT? */
					strut->fmt = FS_FAT32;
					break;
				}
			}
			if (!(strut->opt & FM_FAT)) {
				fResult = FR_INVALID_PARAMETER;
				delete strut;
				callNextCallback();
				return;
			} /* no-FAT? */
			strut->fmt = FS_FAT16;
		} while (0);

#if _NB_FS_EXFAT
		if (strut->fmt == FS_EXFAT) { /* Create an exFAT volume */

			if (strut->sz_vol < 0x1000) {
				fResult = FR_MKFS_ABORTED;
				delete strut;
				callNextCallback();
				return;
			} /* Too small volume? */
#if _NB_USE_TRIM
#error
		tbl[0] = b_vol;
		tbl[1] = b_vol + strut->pau - 1; /* Inform the device the volume area may be erased */
		disk_ioctl(pdrv, CTRL_TRIM, tbl);
#endif
			/* Determine FAT location, data location and number of clusters */
			if (!strut->au) { /* au auto-selection */
				strut->au = 8;
				if (strut->sz_vol >= 0x80000)
					strut->au = 64; /* >= 512Ks */
				if (strut->sz_vol >= 0x4000000)
					strut->au = 256; /* >= 64Ms */
			}
			strut->b_fat = strut->b_vol + 32; /* FAT start at offset 32 */
			strut->sz_fat = ((strut->sz_vol / strut->au + 2) * 4 + strut->ss - 1) / strut->ss; /* Number of FAT sectors */
			strut->b_data = (strut->b_fat + strut->sz_fat + strut->sz_blk - 1) & ~(strut->sz_blk - 1); /* Align data area to the erase block boundary */
			if (strut->b_data >= strut->sz_vol / 2) {
				fResult = FR_MKFS_ABORTED;
				delete strut;
				callNextCallback();
				return;
			} /* Too small volume? */
			strut->n_clst = (strut->sz_vol - (strut->b_data - strut->b_vol)) / strut->au; /* Number of clusters */
			if (strut->n_clst < 16) {
				fResult = FR_MKFS_ABORTED;
				delete strut;
				callNextCallback();
				return;
			} /* Too few clusters? */
			if (strut->n_clst > MAX_EXFAT) {
				fResult = FR_MKFS_ABORTED;
				delete strut;
				callNextCallback();
				return;
			} /* Too many clusters? */

			strut->szb_bit = (strut->n_clst + 7) / 8; /* Size of allocation bitmap */
			strut->tbl[0] = (strut->szb_bit + strut->au * strut->ss - 1) / (strut->au * strut->ss); /* Number of allocation bitmap clusters */

			/* Create a compressed up-case table */
			strut->sect = strut->b_data + strut->au * strut->tbl[0]; /* Table start sector */
			strut->sum = 0; /* Table checksum to be stored in the 82 entry */
			strut->st = strut->si = strut->i = strut->j = strut->szb_case = 0;
			f_mkfs_g_loop(strut);
			return;
		} else
#endif /* _NB_FS_EXFAT */
		{ /* Create an FAT12/16/32 volume */
			do {
				strut->pau = strut->au;
				/* Pre-determine number of clusters and FAT sub-type */
				if (strut->fmt == FS_FAT32) { /* FAT32 volume */
					if (!strut->pau) { /* au auto-selection */
						strut->n = strut->sz_vol / 0x20000; /* Volume size in unit of 128KS */
						for (strut->i = 0, strut->pau = 1; cst32[strut->i] && cst32[strut->i] <= strut->n; strut->i++, strut->pau <<= 1)
							; /* Get from table */
					}
					strut->n_clst = strut->sz_vol / strut->pau; /* Number of clusters */
					strut->sz_fat = (strut->n_clst * 4 + 8 + strut->ss - 1) / strut->ss; /* FAT size [sector] */
					strut->sz_rsv = 32; /* Number of reserved sectors */
					strut->sz_dir = 0; /* No static directory */
					if (strut->n_clst <= MAX_FAT16 || strut->n_clst > MAX_FAT32) {
						fResult = FR_MKFS_ABORTED;
						delete strut;
						callNextCallback();
						return;
					}
				} else { /* FAT12/16 volume */
					if (!strut->pau) { /* au auto-selection */
						strut->n = strut->pau / 0x1000; /* Volume size in unit of 4KS */
						for (strut->i = 0, strut->pau = 1; cst[strut->i] && cst[strut->i] <= strut->n; strut->i++, strut->pau <<= 1)
							; /* Get from table */
					}
					strut->n_clst = strut->pau / strut->pau;
					if (strut->n_clst > MAX_FAT12) {
						strut->n = strut->n_clst * 2 + 4; /* FAT size [byte] */
					} else {
						strut->fmt = FS_FAT12;
						strut->n = (strut->n_clst * 3 + 1) / 2 + 3; /* FAT size [byte] */
					}
					strut->sz_fat = (strut->n + strut->ss - 1) / strut->ss; /* FAT size [sector] */
					strut->sz_rsv = 1; /* Number of reserved sectors */
					strut->sz_dir = (DWORD) strut->n_rootdir * SZDIRE / strut->ss; /* Rootdir size [sector] */
				}
				strut->b_fat = strut->b_vol + strut->sz_rsv; /* FAT base */
				strut->b_data = strut->b_fat + strut->sz_fat * strut->n_fats + strut->sz_dir; /* Data base */

				/* Align data base to erase block boundary (for flash memory media) */
				strut->n = ((strut->b_data + strut->sz_blk - 1) & ~(strut->sz_blk - 1)) - strut->b_data; /* Next nearest erase block from current data base */
				if (strut->fmt == FS_FAT32) { /* FAT32: Move FAT base */
					strut->sz_rsv += strut->n;
					strut->b_fat += strut->n;
				} else { /* FAT12/16: Expand FAT size */
					strut->sz_fat += strut->n / strut->n_fats;
				}

				/* Determine number of clusters and final check of validity of the FAT sub-type */
				if (strut->pau < strut->b_data + strut->pau * 16 - strut->b_vol) {
					fResult = FR_MKFS_ABORTED;
					delete strut;
					callNextCallback();
					return;
				} /* Too small volume */
				strut->n_clst = (strut->pau - strut->sz_rsv - strut->sz_fat * strut->n_fats - strut->sz_dir) / strut->pau;
				if (strut->fmt == FS_FAT32) {
					if (strut->n_clst <= MAX_FAT16) { /* Too few clusters for FAT32 */
						if (!strut->au && (strut->au = strut->pau / 2) != 0) {
							continue;
						} /* Adjust cluster size and retry */
						fResult = FR_MKFS_ABORTED;
						delete strut;
						callNextCallback();
						return;
					}
				}
				if (strut->fmt == FS_FAT16) {
					if (strut->n_clst > MAX_FAT16) { /* Too many clusters for FAT16 */
						if (!strut->au && (strut->pau * 2) <= 64) {
							strut->au = strut->pau * 2;
							{
								continue;
							} /* Adjust cluster size and retry */
						}
						if ((strut->opt & FM_FAT32)) {
							strut->fmt = FS_FAT32;
							{
								continue;
							} /* Switch type to FAT32 and retry */
						}
						if (!strut->au && (strut->au = strut->pau * 2) <= 128) {
							continue;
						} /* Adjust cluster size and retry */
						fResult = FR_MKFS_ABORTED;
						delete strut;
						callNextCallback();
						return;
					}
					if (strut->n_clst <= MAX_FAT12) { /* Too few clusters for FAT16 */
						if (!strut->au && (strut->au = strut->pau * 2) <= 128) {
							continue;
						} /* Adjust cluster size and retry */
						fResult = FR_MKFS_ABORTED;
						delete strut;
						callNextCallback();
						return;
					}
				}
				if (strut->fmt == FS_FAT12 && strut->n_clst > MAX_FAT12) {
					fResult = FR_MKFS_ABORTED;
					delete strut;
					callNextCallback();
					return;
				} /* Too many clusters for FAT12 */

				/* Ok, it is the valid cluster configuration */
				break;
			} while (1);

#if _NB_USE_TRIM
#error
		tbl[0] = strut->b_vol;
		tbl[1] = strut->b_vol + strut->pau - 1; /* Inform the device the volume area can be erased */
		disk_ioctl(pdrv, CTRL_TRIM, tbl);
#endif
			/* Create FAT VBR */
			mem_set(strut->buf, 0, strut->ss);
			mem_cpy(strut->buf + BS_JmpBoot, "\xEB\xFE\x90"
					"MSDOS5.0", 11); /* Boot jump code (x86), OEM name */
			st_word(strut->buf + BPB_BytsPerSec, strut->ss); /* Sector size [byte] */
			strut->buf[BPB_SecPerClus] = (BYTE) strut->pau; /* Cluster size [sector] */
			st_word(strut->buf + BPB_RsvdSecCnt, (WORD) strut->sz_rsv); /* Size of reserved area */
			strut->buf[BPB_NumFATs] = (BYTE) strut->n_fats; /* Number of FATs */
			st_word(strut->buf + BPB_RootEntCnt, (WORD) ((strut->fmt == FS_FAT32) ? 0 : strut->n_rootdir)); /* Number of root directory entries */
			if (strut->pau < 0x10000) {
				st_word(strut->buf + BPB_TotSec16, (WORD) strut->pau); /* Volume size in 16-bit LBA */
			} else {
				st_dword(strut->buf + BPB_TotSec32, strut->pau); /* Volume size in 32-bit LBA */
			}
			strut->buf[BPB_Media] = 0xF8; /* Media descriptor byte */
			st_word(strut->buf + BPB_SecPerTrk, 63); /* Number of sectors per track (for int13) */
			st_word(strut->buf + BPB_NumHeads, 255); /* Number of heads (for int13) */
			st_dword(strut->buf + BPB_HiddSec, strut->b_vol); /* Volume offset in the physical drive [sector] */
			if (strut->fmt == FS_FAT32) {
				st_dword(strut->buf + BS_VolID32, GET_FATTIME()); /* VSN */
				st_dword(strut->buf + BPB_FATSz32, strut->sz_fat); /* FAT size [sector] */
				st_dword(strut->buf + BPB_RootClus32, 2); /* Root directory cluster # (2) */
				st_word(strut->buf + BPB_FSInfo32, 1); /* Offset of FSINFO sector (VBR + 1) */
				st_word(strut->buf + BPB_BkBootSec32, 6); /* Offset of backup VBR (VBR + 6) */
				strut->buf[BS_DrvNum32] = 0x80; /* Drive number (for int13) */
				strut->buf[BS_BootSig32] = 0x29; /* Extended boot signature */
				mem_cpy(strut->buf + BS_VolLab32, "NO NAME    "
						"FAT32   ", 19); /* Volume label, FAT signature */
			} else {
				st_dword(strut->buf + BS_VolID, GET_FATTIME()); /* VSN */
				st_word(strut->buf + BPB_FATSz16, (WORD) strut->sz_fat); /* FAT size [sector] */
				strut->buf[BS_DrvNum] = 0x80; /* Drive number (for int13) */
				strut->buf[BS_BootSig] = 0x29; /* Extended boot signature */
				mem_cpy(strut->buf + BS_VolLab, "NO NAME    "
						"FAT     ", 19); /* Volume label, FAT signature */
			}
			st_word(strut->buf + BS_55AA, 0xAA55); /* Signature (offset is fixed here regardless of sector size) */
			voidPtr = strut;
			disk_write(strut->pdrv, strut->buf, strut->b_vol, 1, [](DRESULT dRes) {
				f_mkfs_strut *strut = (f_mkfs_strut*) voidPtr;
				if (dRes != RES_OK) {
					fResult = FR_DISK_ERROR;
					delete strut;
					callNextCallback();
					return;
				} /* Write it to the VBR sector */

				/* Create FSINFO record if needed */
				if (strut->fmt == FS_FAT32) {
					voidPtr = strut;
					disk_write(strut->pdrv, strut->buf, strut->b_vol + 6, 1, /* Write backup VBR (VBR + 6) */
					[](DRESULT dres) {
						f_mkfs_strut *strut = (f_mkfs_strut*) voidPtr;
						mem_set(strut->buf, 0, strut->ss);
						st_dword(strut->buf + FSI_LeadSig, 0x41615252);
						st_dword(strut->buf + FSI_StrucSig, 0x61417272);
						st_dword(strut->buf + FSI_Free_Count, strut->n_clst - 1); /* Number of free clusters */
						st_dword(strut->buf + FSI_Nxt_Free, 2); /* Last allocated cluster# */
						st_word(strut->buf + BS_55AA, 0xAA55);
						voidPtr = strut;
						disk_write(strut->pdrv, strut->buf, strut->b_vol + 7, 1, /* Write backup FSINFO (VBR + 7) */
						[](DRESULT dres) {
							f_mkfs_strut *strut = (f_mkfs_strut*) voidPtr;
							voidPtr = strut;
							disk_write(strut->pdrv, strut->buf, strut->b_vol + 1, 1, [](DRESULT dres) {
								f_mkfs_strut *strut = (f_mkfs_strut*) voidPtr;
								f_mkfs_a_a(strut);
								return;
							}); /* Write original FSINFO (VBR + 1) */
							return;
						});
						return;
					});
					return;
				}
				f_mkfs_a_a(strut);
				return;
			});
			return;
		}
		f_mkfs_b(strut);
	}

	void f_mkfs(const TCHAR *pathi, /* Logical drive number */
	BYTE opti, /* Format option */
	DWORD aui, /* Size of allocation unit (cluster) [byte] */
	void *worki, /* Pointer to working buffer */
	UINT leni /* Size of working buffer */
	, void (*callback)(void*)) {
		addCallback(callback, &fResult);
		f_mkfs_strut *s = new f_mkfs_strut();
		s->path = pathi;
		s->opt = opti;
		s->au = aui;
		s->work = worki;
		s->len = leni;

		/* Check mounted drive and clear work area */
		s->vol = get_ldnumber(&s->path); /* Get target logical drive */
		if (s->vol < 0) {
			fResult = FR_INVALID_DRIVE;
			delete s;
			callNextCallback();
			return;
		}
		if (FatFs[s->vol])
			FatFs[s->vol]->fs_type = 0; /* Clear the volume */
		s->pdrv = LD2PD(s->vol); /* Physical drive */
		s->part = LD2PT(s->vol); /* Partition (0:create as new, 1-4:get from partition table) */

		/* Check physical drive status */
		if (s->stat & STA_NOT_INITIALIZED) {
			fResult = FR_NOT_READY;
			delete s;
			callNextCallback();
			return;
		}
		if (s->stat & STA_WRITE_PROTECTED) {
			fResult = FR_WRITE_PROTECTED;
			delete s;
			callNextCallback();
			return;
		}
		if (disk_ioctl(s->pdrv, GET_BLOCK_SIZE, &s->sz_blk) != RES_OK || !s->sz_blk || s->sz_blk > 32768 || (s->sz_blk & (s->sz_blk - 1))) {
			s->sz_blk = 1;
		} /* Erase block to align data area */
#if _NB_MAX_SS != _NB_MIN_SS /* Get sector size of the medium if variable sector size cfg. */
	if (disk_ioctl(s->pdrv, GET_SECTOR_SIZE, &s->ss) != RES_OK)
	{
		fResult = FR_DISK_ERROR;
		delete strut;
		callNextCallback();
		return;
	}
	if (s->ss > _NB_MAX_SS || s->ss < _NB_MIN_SS || (s->ss & (s->ss - 1)))
	{
		fResult = FR_DISK_ERROR;
		delete strut;
		callNextCallback();
		return;
	}
#else
		s->ss = _NB_MAX_SS;
#endif
		if ((s->au != 0 && s->au < s->ss) || s->au > 0x1000000 || (s->au & (s->au - 1))) {
			fResult = FR_INVALID_PARAMETER;
			delete s;
			callNextCallback();
			return;
		} /* Check if au is valid */
		s->au /= s->ss; /* Cluster size in unit of sector */

		/* Get working buffer */
		s->buf = (BYTE*) s->work; /* Working buffer */
		s->sz_buf = s->len / s->ss; /* Size of working buffer (sector) */
		s->szb_buf = s->sz_buf * s->ss; /* Size of working buffer (byte) */
		if (!s->szb_buf) {
			fResult = FR_MKFS_ABORTED;
			delete s;
			callNextCallback();
			return;
		}

		/* Determine where the volume to be located (b_vol, strut->pau) */
		if (_NB_MULTI_PARTITION && s->part != 0) {
			/* Get partition information from partition table in the MBR */
			voidPtr = s;
			disk_read(s->pdrv, s->buf, 0, 1, [](DRESULT dRes) {
				f_mkfs_strut *strut = (f_mkfs_strut*) voidPtr;
				if (dRes != RES_OK) {
					fResult = FR_DISK_ERROR;
					delete strut;
					callNextCallback();
					return;
				} /* Load MBR */
				if (ld_word(strut->buf + BS_55AA) != 0xAA55) {
					fResult = FR_MKFS_ABORTED;
					delete strut;
					callNextCallback();
					return;
				} /* Check if MBR is valid */
				strut->pte = strut->buf + (MBR_Table + (strut->part - 1) * SZ_PTE);
				if (!strut->pte[PTE_System]) {
					fResult = FR_MKFS_ABORTED;
					delete strut;
					callNextCallback();
					return;
				} /* No partition? */
				strut->b_vol = ld_dword(strut->pte + PTE_StLba); /* Get volume start sector */
				strut->pau = ld_dword(strut->pte + PTE_SizLba); /* Get volume size */
				f_mkfs_a(strut);
				return;
			});
			return;
		} else {
			/* Create a single-partition in this function */
			if (disk_ioctl(s->pdrv, GET_SECTOR_COUNT, &s->sz_vol) != RES_OK) {
				fResult = FR_DISK_ERROR;
				delete s;
				callNextCallback();
				return;
			}
			s->b_vol = (s->opt & FM_SFD) ? 0 : 63; /* Volume start sector */
			if (s->sz_vol < s->b_vol) {
				fResult = FR_MKFS_ABORTED;
				delete s;
				callNextCallback();
				return;
			}
			s->sz_vol -= s->b_vol; /* Volume size */
			f_mkfs_a(s);
			return;
		}
	}

#if _NB_MULTI_PARTITION
#error
/*-----------------------------------------------------------------------*/
/* Create partition table on the physical drive                          */
/*-----------------------------------------------------------------------*/

FRESULT f_fdisk(
	BYTE pdrv,		  /* Physical drive number */
	const DWORD *szt, /* Pointer to the size table for each partitions */
	void *work		  /* Pointer to the working buffer */
	,
	void (*callback)(void *))
{
	addCallback(callback, data);
	UINT i, n, sz_cyl, tot_cyl, b_cyl, e_cyl, p_cyl;
	BYTE s_hd, e_hd, *p, *buf = (BYTE *)work;
	DSTATUS stat;
	DWORD sz_disk, sz_part, s_part;

	stat = disk_initialize(pdrv);
	if (stat & STA_NOT_INITIALIZED)
	{
		return FR_NOT_READY;
	}
	if (stat & STA_WRITE_PROTECTED)
	{
		return FR_WRITE_PROTECTED;
	}
	if (disk_ioctl(pdrv, GET_SECTOR_COUNT, &sz_disk))
	{
		return FR_DISK_ERROR;
	}

	/* Determine the CHS without any consideration of the drive geometry */
	for (n = 16; n < 256 && sz_disk / n / 63 > 1024; n *= 2)
		;
	if (n == 256)
	{
		n--;
	}
	e_hd = n - 1;
	sz_cyl = 63 * n;
	tot_cyl = sz_disk / sz_cyl;

	/* Create partition table */
	mem_set(buf, 0, _NB_MAX_SS);
	p = buf + MBR_Table;
	b_cyl = 0;
	for (i = 0; i < 4; i++, p += SZ_PTE)
	{
		p_cyl = (szt[i] <= 100U) ? (DWORD)tot_cyl * szt[i] / 100 : szt[i] / sz_cyl; /* Number of cylinders */
		if (!p_cyl)
		{
			continue;
		}
		s_part = (DWORD)sz_cyl * b_cyl;
		sz_part = (DWORD)sz_cyl * p_cyl;
		if (i == 0)
		{ /* Exclude first track of cylinder 0 */
			s_hd = 1;
			s_part += 63;
			sz_part -= 63;
		}
		else
		{
			s_hd = 0;
		}
		e_cyl = b_cyl + p_cyl - 1; /* End cylinder */
		if (e_cyl >= tot_cyl)
		{
			return FR_INVALID_PARAMETER;
		}

		/* Set partition table */
		p[1] = s_hd;					  /* Start head */
		p[2] = (BYTE)((b_cyl >> 2) + 1);  /* Start sector */
		p[3] = (BYTE)b_cyl;				  /* Start cylinder */
		p[4] = 0x07;					  /* System type (temporary setting) */
		p[5] = e_hd;					  /* End head */
		p[6] = (BYTE)((e_cyl >> 2) + 63); /* End sector */
		p[7] = (BYTE)e_cyl;				  /* End cylinder */
		st_dword(p + 8, s_part);		  /* Start sector in LBA */
		st_dword(p + 12, sz_part);		  /* Number of sectors */

		/* Next partition */
		b_cyl += p_cyl;
	}
	st_word(p, 0xAA55);

	/* Write it to the MBR */
	return (disk_write(pdrv, buf, 0, 1) != RES_OK || disk_ioctl(pdrv, CTRL_SYNC, 0) != RES_OK) ? FR_DISK_ERROR : FR_OK;
}

#endif /* _MULTI_PARTITION */
#endif /* _USE_MKFS && !_FS_READONLY */

#if _NB_USE_STRFUNC
	/*-----------------------------------------------------------------------*/
	/* Get a string from the file                                            */
	/*-----------------------------------------------------------------------*/
	struct f_gets_strut {
		TCHAR *buff; /* Pointer to the string buffer to read */
		int len; /* Size of string buffer (characters) */
		FIL *fp; /* Pointer to the file object */
		int n = 0;
		TCHAR c, *p = buff;
		BYTE s[2];
		UINT rc;
	};

	void f_gets_loop(f_gets_strut *strut) {

		if (strut->n < strut->len - 1) { /* Read characters until buffer gets filled */
#if _NB_LFN_UNICODE
#if _NB_STRF_ENCODE == 3 /* Read a character in UTF-8 */
#error
		f_read(fp, s, 1, &rc);
		if (rc != 1)
		{
			break;
		}
		c = s[0];
		if (c >= 0x80)
		{
			if (c < 0xC0)
			{
				continue;
			} /* Skip stray trailer */
			if (c < 0xE0)
			{ /* Two-byte sequence (0x80-0x7FF) */
				f_read(fp, s, 1, &rc);
				if (rc != 1)
				{
					break;
				}
				c = (c & 0x1F) << 6 | (s[0] & 0x3F);
				if (c < 0x80)
					c = '?'; /* Reject invalid code range */
			}
			else
			{
				if (c < 0xF0)
				{ /* Three-byte sequence (0x800-0xFFFF) */
					f_read(fp, s, 2, &rc);
					if (rc != 2)
					{
						break;
					}
					c = c << 12 | (s[0] & 0x3F) << 6 | (s[1] & 0x3F);
					if (c < 0x800)
					{
						c = '?';
					} /* Reject invalid code range */
				}
				else
				{ /* Reject four-byte sequence */
					c = '?';
				}
			}
		}
#elif _STRF_ENCODE == 2 /* Read a character in UTF-16BE */
		f_read(fp, s, 2, &rc);
		if (rc != 2)
		{
			break;
		}
		c = s[1] + (s[0] << 8);
#elif _STRF_ENCODE == 1 /* Read a character in UTF-16LE */
		f_read(fp, s, 2, &rc);
		if (rc != 2)
		{
			break;
		}
		c = s[0] + (s[1] << 8);
#else					/* Read a character in ANSI/OEM */
		f_read(fp, s, 1, &rc);
		if (rc != 1)
		{
			break;
		}
		c = s[0];
		if (IsDBCS1(c))
		{
			f_read(fp, s, 1, &rc);
			if (rc != 1)
			{
				break;
			}
			c = (c << 8) + s[0];
		}
		c = ff_convert(c, 1); /* OEM -> Unicode */
		if (!c)
			c = '?';
#endif
#else /* Read a character without conversion */
			f_read(strut->fp, strut->s, 1, &strut->rc, [](void *data) {
				f_gets_strut *strut = (f_gets_strut*) data;
				if (strut->rc != 1) {
					*strut->p = 0;
					strut->p = strut->n ? strut->buff : 0; /* When no data read (eof or error), return with error. */
					delete strut;
					callNextCallback();
					return;
				}
				strut->c = strut->s[0];
#endif
				if (_NB_USE_STRFUNC == 2 && strut->c == '\r') {
					f_gets_loop(strut);
					return;
				} /* Strip '\r' */
				*strut->p++ = strut->c;
				strut->n++;
				if (strut->c == '\n') {
					*strut->p = 0;
					strut->p = strut->n ? strut->buff : 0; /* When no data read (eof or error), return with error. */
					delete strut;
					callNextCallback();
					return;
				} /* Break on EOL */
				f_gets_loop(strut);
				return;
			},&strut);
			return;
		}
		*strut->p = 0;
		strut->p = strut->n ? strut->buff : 0; /* When no data read (eof or error), return with error. */
		delete strut;
		callNextCallback();
		return;
	}

	void f_gets(TCHAR *buffi, /* Pointer to the string buffer to read */
	int leni, /* Size of string buffer (characters) */
	FIL *fpi /* Pointer to the file object */
	, void (*callback)(void*)) {
		f_gets_strut *s = new f_gets_strut();
		addCallback(callback, s->p);
		s->buff = buffi;
		s->len = leni;
		s->fp = fpi;
		s->n = 0;
		s->p = buffi;

		f_gets_loop(s);
		return;
	}

#if !_NB_FS_READONLY
#include <stdarg.h>
	/*-----------------------------------------------------------------------*/
	/* Put a character to the file                                           */
	/*-----------------------------------------------------------------------*/

	typedef struct {
		FIL *fp; /* Ptr to the writing file */
		int idx, nchr; /* Write index of buf[] (-1:error), number of chars written */
		BYTE buf[64]; /* Write buffer */
	} putbuff;

	struct putc_bfd_strut {
		TCHAR c;
		UINT bw;
		int i;
		putbuff *pb;
	};

	void putc_bfd_a(putc_bfd_strut *strut) {
		strut->i = strut->pb->idx; /* Write index of pb->buf[] */
		if (strut->i < 0) {
			delete strut;
			callNextCallback();
			return;
		}

#if _NB_LFN_UNICODE
#if _NB_STRF_ENCODE == 3 /* Write a character in UTF-8 */
#error
	if (c < 0x80)
	{ /* 7-bit */
		pb->buf[i++] = (BYTE)c;
	}
	else
	{
		if (c < 0x800)
		{ /* 11-bit */
			pb->buf[i++] = (BYTE)(0xC0 | c >> 6);
		}
		else
		{ /* 16-bit */
			pb->buf[i++] = (BYTE)(0xE0 | c >> 12);
			pb->buf[i++] = (BYTE)(0x80 | (c >> 6 & 0x3F));
		}
		pb->buf[i++] = (BYTE)(0x80 | (c & 0x3F));
	}
#elif _STRF_ENCODE == 2 /* Write a character in UTF-16BE */
	pb->buf[i++] = (BYTE)(c >> 8);
	pb->buf[i++] = (BYTE)c;
#elif _STRF_ENCODE == 1 /* Write a character in UTF-16LE */
	pb->buf[i++] = (BYTE)c;
	pb->buf[i++] = (BYTE)(c >> 8);
#else					/* Write a character in ANSI/OEM */
	c = ff_convert(c, 0); /* Unicode -> OEM */
	if (!c)
	{
		c = '?';
	}
	if (c >= 0x100)
		pb->buf[i++] = (BYTE)(c >> 8);
	pb->buf[i++] = (BYTE)c;
#endif
#else /* Write a character without conversion */
		strut->pb->buf[strut->i++] = (BYTE) strut->c;
#endif

		if (strut->i >= (int) (sizeof strut->pb->buf) - 3) { /* Write buffered characters to the file */
			f_write(strut->pb->fp, strut->pb->buf, (UINT) strut->i, &strut->bw, [](void *data) {
				putc_bfd_strut *strut = ((putc_bfd_strut*) data);
				strut->i = (strut->bw == (UINT) strut->i) ? 0 : -1;
				strut->pb->idx = strut->i;
				strut->pb->nchr++;
				delete strut;
				callNextCallback();
				return;
			}, strut);
			return;
		}
		strut->pb->idx = strut->i;
		strut->pb->nchr++;
		delete strut;
		callNextCallback();
		return;
	}

	static void putc_bfd(/* Buffered write with code conversion */
	putbuff *pb, TCHAR c, void (*callback)(void*), void *data) {
		addCallback(callback, data);
		putc_bfd_strut *s = new putc_bfd_strut();
		s->pb = pb;
		s->c = c;

		if (_NB_USE_STRFUNC == 2 && s->c == '\n') { /* LF -> CRLF conversion */
			putc_bfd(s->pb, '\r', [](void *data) {
				putc_bfd_strut *strut = ((putc_bfd_strut*) data);
				putc_bfd_a(strut);
				return;
			}, s);
			return;
		}
		putc_bfd_a(s);
		return;
	}

	struct putc_flush_strut {
		putbuff *pb;
		UINT nw;
	};

	static void putc_flush(/* Flush left characters in the buffer */
	putbuff *pb, void (*callback)(void*), void *data) {
		addCallback(callback, data);
		putc_flush_strut *s = new putc_flush_strut();
		s->pb = pb;

		if (pb->idx >= 0) { /* Flush buffered characters to the file */
			f_write(s->pb->fp, s->pb->buf, (UINT) pb->idx, &s->nw, [](void *data) {
				putc_flush_strut *strut = ((putc_flush_strut*) data);
				if (fResult == FR_OK && (UINT) strut->pb->idx == strut->nw) {
					putc_flush_Value = strut->pb->nchr;
				}
				delete strut;
				callNextCallback();
				return;
			}, s);
			return;
		}
		putc_flush_Value = EOF;
		delete s;
		callNextCallback();
		return;
	}

	static void putc_init(/* Initialize write buffer */
	putbuff *pb, FIL *fp) {
		pb->fp = fp;
		pb->nchr = pb->idx = 0;
	}

	struct f_putc_strut {
		TCHAR c; /* A character to be output */
		FIL *fp; /* Pointer to the file object */
		putbuff pb;
	};

	void f_putc(TCHAR c, /* A character to be output */
	FIL *fp /* Pointer to the file object */
	, void (*callback)(void*)) {
		addCallback(callback, &putc_flush_Value);
		f_putc_strut *s = new f_putc_strut();

		s->c = c;
		s->fp = fp;

		putc_init(&s->pb, s->fp);
		putc_bfd(&s->pb, s->c, [](void *data) {
			f_putc_strut *strut = ((f_putc_strut*) data);
			putc_flush(&strut->pb, [](void *data) {
				f_putc_strut *strut = ((f_putc_strut*) data);
				delete strut;
				callNextCallback();
				return;
			},strut);
			return;
		}, s);
		return; /* Put the character */
	}

	/*-----------------------------------------------------------------------*/
	/* Put a string to the file                                              */
	/*-----------------------------------------------------------------------*/

	struct f_puts_strut {
		const TCHAR *str; /* Pointer to the string to be output */
		FIL *fp; /* Pointer to the file object */
		putbuff pb;
	};

	void f_puts_loop(f_puts_strut *strut) {
		if (*strut->str) {
			putc_bfd(&strut->pb, *strut->str++, [](void *data) {
				f_puts_strut *strut = ((f_puts_strut*) data);
				f_puts_loop(strut);
				return;
			}, strut);
			return;
		} /* Put the string */
		putc_flush(&strut->pb, [](void *data) {
			f_puts_strut *strut = ((f_puts_strut*) data);
			delete strut;
			callNextCallback();
			return;
		}, strut);
		return;
	}

	void f_puts(const TCHAR *str, /* Pointer to the string to be output */
	FIL *fp /* Pointer to the file object */
	, void (*callback)(void*)) {
		addCallback(callback, &fResult);
		f_puts_strut *s = new f_puts_strut();

		s->fp = fp;
		s->str = str;

		putc_init(&s->pb, s->fp);
		f_puts_loop(s);
		return;
	}

/*-----------------------------------------------------------------------*/
/* Put a formatted string to the file                                    */
/*-----------------------------------------------------------------------*/
// struct f_printf_strut
// {
// 	FIL *fp;		  /* Pointer to the file object */
// 	const TCHAR *fmt; /* Pointer to the format string */
// 	va_list arp;
// 	putbuff pb;
// 	BYTE f, r;
// 	UINT i, j, w;
// 	DWORD v;
// 	TCHAR c, d, str[32], *p;
// };
// void f_printf(
// 	FIL *fp, /* Pointer to the file object */
// 	void (*callback)(),
// 	const TCHAR *fmt, /* Pointer to the format string */
// 	...				  /* Optional arguments->.. */
// )
// {
// addCallback(callback, data);
// f_printf_strut *s = new ();
// va_list arp;
// putbuff pb;
// BYTE f, r;
// UINT i, j, w;
// DWORD v;
// TCHAR c, d, str[32], *p;
// putc_init(&pb, fp);
// va_start(arp, fmt);
// for (;;)
// {
// 	c = *fmt++;
// 	if (c == 0)
// 	{
// 		break;
// 	} /* End of string */
// 	if (c != '%')
// 	{ /* Non escape character */
// 		putc_bfd(&pb, c);
// 		continue;
// 	}
// 	w = f = 0;
// 	c = *fmt++;
// 	if (c == '0')
// 	{ /* Flag: '0' padding */
// 		f = 1;
// 		c = *fmt++;
// 	}
// 	else
// 	{
// 		if (c == '-')
// 		{ /* Flag: left justified */
// 			f = 2;
// 			c = *fmt++;
// 		}
// 	}
// 	while (IsDigit(c))
// 	{ /* Precision */
// 		w = w * 10 + c - '0';
// 		c = *fmt++;
// 	}
// 	if (c == 'l' || c == 'L')
// 	{ /* Prefix: Size is long int */
// 		f |= 4;
// 		c = *fmt++;
// 	}
// 	if (!c)
// 	{
// 		break;
// 	}
// 	d = c;
// 	if (IsLower(d))
// 	{
// 		d -= 0x20;
// 	}
// 	switch (d)
// 	{		  /* Type is->.. */
// 	case 'S': /* String */
// 		p = va_arg(arp, TCHAR *);
// 		for (j = 0; p[j]; j++)
// 			;
// 		if (!(f & 2))
// 		{
// 			while (j++ < w)
// 			{
// 				putc_bfd(&pb, ' ');
// 			}
// 		}
// 		while (*p)
// 		{
// 			putc_bfd(&pb, *p++);
// 		}
// 		while (j++ < w)
// 		{
// 			putc_bfd(&pb, ' ');
// 		}
// 		continue;
// 	case 'C': /* Character */
// 		putc_bfd(&pb, (TCHAR)va_arg(arp, int));
// 		continue;
// 	case 'B': /* Binary */
// 		r = 2;
// 		break;
// 	case 'O': /* Octal */
// 		r = 8;
// 		break;
// 	case 'D': /* Signed decimal */
// 	case 'U': /* Unsigned decimal */
// 		r = 10;
// 		break;
// 	case 'X': /* Hexdecimal */
// 		r = 16;
// 		break;
// 	default: /* Unknown type (pass-through) */
// 		putc_bfd(&pb, c);
// 		continue;
// 	}
// 	/* Get an argument and put it in numeral */
// 	v = (f & 4) ? (DWORD)va_arg(arp, long) : ((d == 'D') ? (DWORD)(long)va_arg(arp, int) : (DWORD)va_arg(arp, unsigned int));
// 	if (d == 'D' && (v & 0x80000000))
// 	{
// 		v = 0 - v;
// 		f |= 8;
// 	}
// 	i = 0;
// 	do
// 	{
// 		d = (TCHAR)(v % r);
// 		v /= r;
// 		if (d > 9)
// 		{
// 			d += (c == 'x') ? 0x27 : 0x07;
// 		}
// 		str[i] = d + '0';
// 		i++;
// 	} while (v && i < sizeof str / sizeof str[0]);
// 	if (f & 8)
// 	{
// 		str[i++] = '-';
// 	}
// 	j = i;
// 	d = (f & 1) ? '0' : ' ';
// 	while (!(f & 2) && j < w)
// 	{
// 		j++;
// 		putc_bfd(&pb, d);
// 	}
// 	do
// 	{
// 		i--;
// 		putc_bfd(&pb, str[i]);
// 	} while (i);
// 	while (j++ < w)
// 	{
// 		putc_bfd(&pb, d);
// 	}
// }
// va_end(arp);
// return putc_flush(&pb);
// }
#endif /* !_FS_READONLY */
#endif /* _USE_STRFUNC */
}
