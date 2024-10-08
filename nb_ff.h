/*----------------------------------------------------------------------------/
/  FatFs - Generic FAT file system module  R0.12c                             /
/-----------------------------------------------------------------------------/
/
/ Copyright (C) 2017, ChaN, all right reserved.
/
/ FatFs module is an open source software. Redistribution and use of FatFs in
/ source and binary forms, with or without modification, are permitted provided
/ that the following condition is met:

/ 1. Redistributions of source code must retain the above copyright notice,
/    this condition and the following disclaimer.
/
/ This software is provided by the copyright holder and contributors "AS IS"
/ and any warranties related to this software are DISCLAIMED.
/ The copyright owner or contributors be NOT LIABLE for any damages caused
/ by use of this software.
/----------------------------------------------------------------------------*/

#ifndef _NB_FATFS
#define _NB_FATFS 68300 /* Revision ID */

#include "nb_integer.h" /* Basic integer types */
#include "nb_ffconf.h"	/* FatFs configuration options */

namespace FatFS_NB
{

#if _NB_FATFS != _NB_FFCONF
#error Wrong configuration file (ffconf.h).
#endif

	/* Definitions of volume management */

#if _NB_MULTI_PARTITION /* Multiple partition configuration */
	typedef struct
	{
		BYTE pd; /* Physical drive number */
		BYTE pt; /* Partition: 0:Auto detect, 1-4:Forced partition) */
	} PARTITION;
	extern PARTITION VolToPart[]; /* Volume - Partition resolution table */
#endif

	/* Type of path name strings on FatFs API */

#if _NB_LFN_UNICODE /* Unicode (UTF-16) string */
#if _NB_USE_LFN == 0
#error _LFN_UNICODE must be 0 at non-LFN cfg.
#endif
#ifndef _INC_TCHAR
	typedef WCHAR TCHAR;
#define _T(x) L##x
#define _TEXT(x) L##x
#endif
#else /* ANSI/OEM string */
#ifndef _INC_TCHAR
	typedef char TCHAR;
#define _T(x) x
#define _TEXT(x) x
#endif
#endif

	/* Type of file size variables */

#if _NB_FS_EXFAT
#if _NB_USE_LFN == 0
#error LFN must be enabled when enable exFAT
#endif
	typedef QWORD FSIZE_t;
#else
	typedef DWORD FSIZE_t;
#endif

	/* File system object structure (FATFS) */
	typedef struct
	{
		BYTE fs_type;	/* File system type (0:N/A) */
		BYTE driveNumber;		/* Physical drive number */
		BYTE n_fats;	/* Number of FATs (1 or 2) */
		BYTE wflag;		/* win[] flag (b0:dirty) */
		BYTE fsi_flag;	/* FSINFO flags (b7:disabled, b0:dirty) */
		WORD id;		/* File system mount ID */
		WORD numberOfRootDirectoryEntries; /* Number of root directory entries (FAT12/16) */
		WORD clusterSize;		/* Cluster size [sectors] */
#if _NB_MAX_SS != _NB_MIN_SS
		WORD sectorSize; /* Sector size (512, 1024, 2048 or 4096) */
#endif
#if _NB_USE_LFN != 0
		WCHAR *lfnBuffer; /* LFN working buffer */
#endif
#if _NB_FS_EXFAT
		BYTE *dirBuffer; /* Directory entry block scratchpad buffer */
#endif
#if _NB_FS_REENTRANT
		_NB_SYNC_t sobj; /* Identifier of sync object */
#endif
#if !_NB_FS_READONLY
		DWORD lastCluster; /* Last allocated cluster */
		DWORD numberOfFreeClusters; /* Number of free clusters */
#endif
#if _NB_FS_RPATH != 0
		DWORD cdir; /* Current directory start cluster (0:root) */
#if _NB_FS_EXFAT
		DWORD cdc_scl;	/* Containing directory start cluster (invalid when cdir is 0) */
		DWORD cdc_size; /* b31-b8:Size of containing directory, b7-b0: Chain status */
		DWORD cdc_ofs;	/* Offset in the containing directory (invalid when cdir is 0) */
#endif
#endif
		DWORD numberOfFatEntries;		  /* Number of FAT entries (number of clusters + 2) */
		DWORD fatSectorSize;		  /* Size of an FAT [sectors] */
		DWORD volBaseSector;		  /* Volume base sector */
		DWORD fatBaseSector;		  /* FAT base sector */
		DWORD dirBaseSector;		  /* Root directory base sector/cluster */
		DWORD dataBaseSector;		  /* Data base sector */
		DWORD winSector;		  /* Current sector appearing in the win[] */
		BYTE win[_NB_MAX_SS]; /* Disk access window for Directory, FAT (and file data at tiny cfg) */
	} FATFS;

	/* Object ID and allocation information (_FDID) */
	typedef struct
	{
		FATFS *fs;		 /* Pointer to the owner file system object */
		WORD id;		 /* Owner file system mount ID */
		BYTE attr;		 /* Object attribute */
		BYTE stat;		 /* Object chain status (b1-0: =0:not contiguous, =2:contiguous (no data on FAT), =3:flagmented in this session, b2:sub-directory stretched) */
		DWORD startCluster;	 /* Object start cluster (0:no cluster or root directory) */
		FSIZE_t objSize; /* Object size (valid when sclust != 0) */
#if _NB_FS_EXFAT
		DWORD firstFragSize; /* Size of first fragment, clusters - 1 (valid when stat == 3) */
		DWORD lastFragSize; /* Size of last fragment needs to be written (valid when not zero) */
		DWORD containingDirectoryStartCluster;  /* Containing directory start cluster (valid when sclust != 0) */
		DWORD sizeOfContainingDirectory; /* b31-b8:Size of containing directory, b7-b0: Chain status (valid when c_scl != 0) */
		DWORD offsetInContainingDirectory;  /* Offset in the containing directory (valid when sclust != 0 and non-directory object) */
#endif
#if _NB_FS_LOCK != 0
		UINT lockid; /* File lock ID origin from 1 (index of file semaphore table Files[]) */
#endif
	} _FDID;

	/* File object structure (FIL) */
	typedef struct
	{
		_FDID obj;	  /* Object identifier (must be the 1st member to detect invalid object pointer) */
		BYTE flag;	  /* File status flags */
		BYTE err;	  /* Abort flag (error code) */
		FSIZE_t fileReadWritePtr; /* File read/write pointer (Zeroed on file open) */
		DWORD currentFilePtrCluster;  /* Current cluster of fpter (invalid when fptr is 0) */
		DWORD sectorNumber;	  /* Sector number appearing in buf[] (0:invalid) */
#if !_NB_FS_READONLY
		DWORD dirSectNumber; /* Sector number containing the directory entry */
		BYTE *dirEntryPtr;	/* Pointer to the directory entry in the win[] */
#endif
#if _NB_USE_FASTSEEK
		DWORD *clusterLinkMapTablePtr; /* Pointer to the cluster link map table (nulled on open, set by application) */
#endif
#if !_NB_FS_TINY
		BYTE buf[_NB_MAX_SS]; /* File private data read/write window */
#endif
	} FIL;

	/* Directory object structure (DIR) */
	typedef struct
	{
		_FDID obj;	 /* Object identifier */
		DWORD currentReadWriteOffset;	 /* Current read/write offset */
		DWORD currentCluster; /* Current cluster */
		DWORD currentSector;	 /* Current sector (0:Read operation has terminated) */
		BYTE *dirPtr;	 /* Pointer to the directory item in the win[] */
		BYTE fn[12]; /* SFN (in/out) {body[8],ext[3],status[1]} */
#if _NB_USE_LFN != 0
		DWORD currentEntryBlockOffset; /* Offset of current entry block being processed (0xFFFFFFFF:Invalid) */
#endif
#if _NB_USE_FIND
		const TCHAR *pat; /* Pointer to the name matching pattern */
#endif
	} DIR;

	/* File information structure (FILINFO) */
	typedef struct
	{
		FSIZE_t fsize; /* File size */
		WORD fdate;	   /* Modified date */
		WORD ftime;	   /* Modified time */
		BYTE fattrib;  /* File attribute */
#if _NB_USE_LFN != 0
		TCHAR altname[13];			  /* Alternative file name */
		TCHAR fname[_NB_MAX_LFN + 1]; /* Primary file name */
#else
		TCHAR fname[13]; /* File name */
#endif
	} FILINFO;

	/* File function return code (FRESULT) */

	typedef enum
	{
		FR_OK = 0,				/* (0) Succeeded */
		FR_DISK_ERROR,			/* (1) A hard error occurred in the low level disk I/O layer */
		FR_INT_ERROR,			/* (2) Assertion failed */
		FR_NOT_READY,			/* (3) The physical drive cannot work */
		FR_NO_FILE,				/* (4) Could not find the file */
		FR_NO_PATH,				/* (5) Could not find the path */
		FR_INVALID_NAME,		/* (6) The path name format is invalid */
		FR_DENIED,				/* (7) Access denied due to prohibited access or directory full */
		FR_EXIST,				/* (8) Access denied due to prohibited access */
		FR_INVALID_OBJECT,		/* (9) The file/directory object is invalid */
		FR_WRITE_PROTECTED,		/* (10) The physical drive is write protected */
		FR_INVALID_DRIVE,		/* (11) The logical drive number is invalid */
		FR_NOT_ENABLED,			/* (12) The volume has no work area */
		FR_NO_FILESYSTEM,		/* (13) There is no valid FAT volume */
		FR_MKFS_ABORTED,		/* (14) The f_mkfs() aborted due to any problem */
		FR_TIMEOUT,				/* (15) Could not get a grant to access the volume within defined period */
		FR_LOCKED,				/* (16) The operation is rejected according to the file sharing policy */
		FR_NOT_ENOUGH_CORE,		/* (17) LFN working buffer could not be allocated */
		FR_TOO_MANY_OPEN_FILES, /* (18) Number of open files > _NB_FS_LOCK */
		FR_INVALID_PARAMETER	/* (19) Given parameter is invalid */
	} FRESULT;

	/*--------------------------------------------------------------*/
	/* FatFs module application interface                           */

	void setPollingMode(bool mode);
	bool getPollingCallReady();
	void pollingModeCall();
	bool getBusy();

	void f_open(FIL *fp, const TCHAR *path, BYTE mode, void (*callback)(void*));							  /* Open or create a file */
	void f_close(FIL *fp, void (*callback)(void*));															  /* Close an open file object */
	void f_read(FIL *fp, void *buff, UINT btr, UINT *br, void (*callback)(void*));							  /* Read data from the file */
	void f_write(FIL *fp, const void *buff, UINT btw, UINT *bw, void (*callback)(void*));					  /* Write data to the file */
	void f_lseek(FIL *fp, FSIZE_t ofs, void (*callback)(void*));											  /* Move file pointer of the file object */
	void f_truncate(FIL *fp, void (*callback)(void*));														  /* Truncate the file */
	void f_sync(FIL *fp, void (*callback)(void*));															  /* Flush cached data of the writing file */
	void f_opendir(DIR *dp, const TCHAR *path, void (*callback)(void*));									  /* Open a directory */
	void f_closedir(DIR *dp, void (*callback)(void*));														  /* Close an open directory */
	void f_readdir(DIR *dp, FILINFO *fno, void (*callback)(void*));											  /* Read a directory item */
	// void f_findfirst(DIR *dp, FILINFO *fno, const TCHAR *path, const TCHAR *pattern, void (*callback)(void*)); /* Find first file */
	// void f_findnext(DIR *dp, FILINFO *fno, void (*callback)(void*));										  /* Find next file */
	void f_mkdir(const TCHAR *path, void (*callback)(void*));												  /* Create a sub directory */
	void f_unlink(const TCHAR *path, void (*callback)(void*));												  /* Delete an existing file or directory */
	void f_rename(const TCHAR *path_old, const TCHAR *path_new, void (*callback)(void*));					  /* Rename/Move a file or directory */
	void f_stat(const TCHAR *path, FILINFO *fno, void (*callback)(void*));									  /* Get file status */
	// void f_chmod(const TCHAR *path, BYTE attr, BYTE mask, void (*callback)(void*));							  /* Change attribute of a file/dir */
	// void f_utime(const TCHAR *path, const FILINFO *fno, void (*callback)(void*));							  /* Change timestamp of a file/dir */
	// void f_chdir(const TCHAR *path, void (*callback)(void*));												  /* Change current directory */
	// void f_chdrive(const TCHAR *path, void (*callback)(void*));												  /* Change current drive */
	// void f_getcwd(TCHAR *buff, UINT len, void (*callback)(void*));											  /* Get current directory */
	void f_getfree(const TCHAR *path, DWORD *nclst, FATFS **fatfs, void (*callback)(void*));					  /* Get number of free clusters on the drive */
	// void f_getlabel(const TCHAR *path, TCHAR *label, DWORD *vsn, void (*callback)(void*));					  /* Get volume label */
	// void f_setlabel(const TCHAR *label, void (*callback)(void*));											  /* Set volume label */
	// void f_forward(FIL *fp, UINT (*func)(const BYTE *, UINT), UINT btf, UINT *bf, void (*callback)(void*));	  /* Forward data to the stream */
	// void f_expand(FIL *fp, FSIZE_t szf, BYTE opt, void (*callback)(void*));									  /* Allocate a contiguous block to the file */
	void f_mount(FATFS *fs, const TCHAR *path, BYTE opt, void (*callback)(void*));		  					  /* Mount/Unmount a logical drive */
	void f_mkfs(const TCHAR *path, BYTE opt, DWORD au, void *work, UINT len, void (*callback)(void*));		  /* Create a FAT volume */
	// void f_fdisk(BYTE pdrv, const DWORD *szt, void *work, void (*callback)(void*));							  /* Divide a physical drive into some partitions */
	void f_putc(TCHAR c, FIL *fp, void (*callback)(void*));													  /* Put a character to the file */
	void f_puts(const TCHAR *str, FIL *cp, void (*callback)(void*));										  /* Put a string to the file */
	// void f_printf(FIL *fp, void (*callback)(void*), const TCHAR *str, ...);									  /* Put a formatted string to the file */
	void f_gets(TCHAR *buff, int len, FIL *fp, void (*callback)(void*));									  /* Get a string from the file */

#define f_eof(fp) ((int)((fp)->fptr == (fp)->obj.objSize))
#define f_error(fp) ((fp)->err)
#define f_tell(fp) ((fp)->fptr)
#define f_size(fp) ((fp)->obj.objSize)
#define f_rewind(fp) f_lseek((fp), 0)
#define f_rewinddir(dp) f_readdir((dp), 0)
#define f_rmdir(path) f_unlink(path)

#ifndef EOF
#define EOF (-1)
#endif

/*--------------------------------------------------------------*/
/* Additional user defined functions                            */

/* RTC function */
#if !_NB_FS_READONLY && !_NB_FS_NORTC
	DWORD get_fattime(void);
#endif

/* Unicode support functions */
#if _NB_USE_LFN != 0						   /* Unicode - OEM code conversion */
	WCHAR ff_convert(WCHAR chr, UINT dir); /* OEM-Unicode bidirectional conversion */
	WCHAR ff_wtoupper(WCHAR chr);		   /* Unicode upper-case conversion */
#if _NB_USE_LFN == 3						   /* Memory functions */
	void *ff_memalloc(UINT msize);		   /* Allocate memory block */
	void ff_memfree(void *mblock);		   /* Free memory block */
#endif
#endif

/* Sync functions */
#if _NB_FS_REENTRANT
	int ff_cre_syncobj(BYTE vol, _NB_SYNC_t *sobj); /* Create a sync object */
	int ff_req_grant(_NB_SYNC_t sobj);				/* Lock sync object */
	void ff_rel_grant(_NB_SYNC_t sobj);				/* Unlock sync object */
	int ff_del_syncobj(_NB_SYNC_t sobj);			/* Delete a sync object */
#endif

/*--------------------------------------------------------------*/
/* Flags and offset address                                     */

/* File access mode and open method flags (3rd argument of f_open) */
#define FA_READ 0x01
#define FA_WRITE 0x02
#define FA_OPEN_EXISTING 0x00
#define FA_CREATE_NEW 0x04
#define FA_CREATE_ALWAYS 0x08
#define FA_OPEN_ALWAYS 0x10
#define FA_OPEN_APPEND 0x30

/* Fast seek controls (2nd argument of f_lseek) */
#define CREATE_LINKMAP ((FSIZE_t)0 - 1)

/* Format options (2nd argument of f_mkfs) */
#define FM_FAT 0x01
#define FM_FAT32 0x02
#define FM_EXFAT 0x04
#define FM_ANY 0x07
#define FM_SFD 0x08

/* Filesystem type (FATFS.fs_type) */
#define FS_FAT12 1
#define FS_FAT16 2
#define FS_FAT32 3
#define FS_EXFAT 4

/* File attribute bits for directory entry (FILINFO.fattrib) */
#define AM_READ_ONLY 0x01 /* Read only */
#define AM_HIDDEN 0x02	  /* Hidden */
#define AM_SYSTEM 0x04	  /* System */
#define AM_DIRECTORY 0x10 /* Directory */
#define AM_ARCHIVE 0x20	  /* Archive */

}

#endif /* _FATFS */
