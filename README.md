# NB_FatFS
Non-Blocking FatFS library, target for embedded devices. Allows FatFS api calls both in polling mode, and asychronous mode, allowing access to a SD Card without blocking or holding up main program.
12-Bit and 16-Bit NOT SUPPORTED


### How To Use
Works similar to the normal FatFS library. There is a slight bit more overhead than the standard FatFS library, though some of it could be reduced with a little testing, eventually. 

Used and tested on STM32H755zi, specifically the CM4 core, with variables stores in the 0x2400000 portion of memory to allow idma access. Currently working using SDMMC with IDMA, purely interrupt driven. 

##IMPORTANT: 
Go to the nb_sd_diskio.cpp file, and modify the read, write, status functions to work with your IO functions. When the IO function is done, it should call the dResCallback, or the dStatCallback. Some non-working examples are shown. Prior to using NB_FatFS functions, **NB_FATFS_LinkDriver(&NB_SD_Driver, SDPath);** needs to be called
too link the driver functions. *Note: Linking Drivers is planned to eventually be removed*
*Note: ioctl was previously an IO function, but was changed to be synchronous, accessing pre fetched data. The expectation is that the user will have the data needed for ioctl ready ahead of NB_FatFS function calls.*

In the "option" folder, ccsbcs.cpp contains "**getFatTime()**". This can be modified to return a timestamp for files. 

Functions are accessed through the "FatFS_NB" namespace and follow the same workflow as FatFS, with a couple notable exceptions.

**FatFS_NB::getBusy()** is used to determine if NB_FatFS is currently performing any operations. If it is, no other functions shall be called, with the exception of the following functions:
    **FatFS_NB::getBusy()**
    **FatFS_NB::getPollingCallReady()**
    **FatFS_NB::pollingModeCall()**
*note: **FatFS_NB::setPollingMode(bool)** should be able to be called in the middle of a FatFS operation without causing any issues, and allow the ability to swap to polling mode mid operation. Warning: only very mild testing was done on this. User to verify their own testing.*

All main functions except the same parameters as FatFS, with an added requirement for a callback function as well. The callback function should be in the form of "**[](void* res){}** ,
res then should be casted like so, "**FatFS_NB::FRESULT result = * (FatFS_NB::FRESULT*)res;**" , and then can be used to determine the success of the operation. 
Here is a simple example of F_Mount:

	Example

                FatFS_NB::f_mount(&fs, SDPath, 1, [](void *result){
                    FatFS_NB::FRESULT res = *(FatFS_NB::FRESULT*)result;
                            if(res != FatFS_NB::FR_OK)
                            {
                                // Good
                            }else{
                                // Bad
                            }
                });

To enable polling mode call **FatFS_NB::setPollingMode(true)**, or to disable it, **FatFS_NB::setPollingMode(true)**;
    When polling mode is on, the expectation is for **FatFS_NB::getBusy()**, **FatFS_NB::getPollingCallReady()**, and **FatFS_NB::pollingModeCall()** to be polled.
    **FatFS_NB::getBusy()** is used to determine if NB_FatFS is still working on an operation, or if it has finished. 
    **FatFS_NB::getPollingCallReady()** is used to determine if an IO function has returned, and the next portion of the operation needs to be executed.
    **FatFS_NB::pollingModeCall()** executes the next portion of the operation, if such is ready. **FatFS_NB::getPollingCallReady()** is NOT required to be called prior, and is safe to repeatedly call

    Example: 
                FatFS_NB::setPollingMode(true);
                FatFS_NB::f_mount(&fs, SDPath, 1, [](void *result){
                    FatFS_NB::FRESULT res = *(FatFS_NB::FRESULT*)result;
                            if(res != FatFS_NB::FR_OK)
                            {
                                // Good
                            }else{
                                // Bad
                            }
                });

                while(FatFS_NB::getBusy()){
                    if(FatFS_NB::getPollingCallReady()){ // This if statement and FatFS_NB::getPollingCallReady() is optional.
                        FatFS_NB::pollingModeCall();
                    }
                }

                // Operation is now finished

*Importante Note: The "nb_ffconf.h" file contains the configurations for NB_FatFS. It has been specifically setup for the exact configurations included, and most likely would not work correctly with other configurations. If there is interest in other configurations, I will add them as requested. 

The following functions are a list of all the FatFS functions, plus the polling functions. Note that the commented out functions are not currently set up to work for NB_FatFS.
*note: As I had no use for the commented out functions, I didnt not modify them to work. If requested though, I can finish setting those functions up, if someone else would be willing to do the testing of them*

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



*Note: For any looking through the files, I apologize for some of the formatting. My code editor did not want to format the file correctly and I did not want to spend the time doing so manually. 
