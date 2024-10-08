/*-----------------------------------------------------------------------*/
/* Low level disk I/O module skeleton for FatFs     (C)ChaN, 2017        */
/*                                                                       */
/*   Portions COPYRIGHT 2017 STMicroelectronics                          */
/*   Portions Copyright (C) 2017, ChaN, all right reserved               */
/*-----------------------------------------------------------------------*/
/* If a working storage control module is available, it should be        */
/* attached to the FatFs via a glue function rather than modifying it.   */
/* This is an example of glue functions to attach various existing      */
/* storage control modules to the FatFs module with a defined API.       */
/*-----------------------------------------------------------------------*/

/* Includes ------------------------------------------------------------------*/
#include "nb_diskio.h"
#include "nb_ff_gen_drv.h"

#if defined ( __GNUC__ )
#ifndef __weak
#define __weak __attribute__((weak))
#endif
#endif

/* Private typedef -----------------------------------------------------------*/
/* Private define ------------------------------------------------------------*/
/* Private variables ---------------------------------------------------------*/
extern Disk_drvTypeDef  disk;

// static DSTATUS dStatus = 0;
// static DRESULT dResult = RES_NOT_READY;
// static void (*callbackHolder)();
// static BYTE pdrvHolder;

/* Private function prototypes -----------------------------------------------*/
/* Private functions ---------------------------------------------------------*/

// DSTATUS getDSTATUS(){
//   return dStatus;
// }

// DRESULT getDRESULT(){
//   return dResult;
// }

/**
  * @brief  Gets Disk Status
  * @param  pdrv: Physical drive number (0..)
  * @retval DSTATUS: Operation status
  */
void disk_status (
	BYTE pdrv,		/* Physical drive number to identify the drive */
  void(*callback)(DSTATUS)
)
{
  
  disk.drv[pdrv]->disk_status(callback);
  // return stat;
}

// /**
//   * @brief  Initializes a Drive
//   * @param  pdrv: Physical drive number (0..)
//   * @retval DSTATUS: Operation status
//   */
// void disk_initialize (
// 	BYTE pdrv,				/* Physical drive nmuber to identify the drive */
//   void(*callback)()
// )
// {
//   callbackHolder = callback;
//   pdrvHolder = pdrv;

//   if(disk.is_initialized[pdrv] == 0)
//   {
//     disk.drv[pdrv]->disk_initialize(
//   [](DSTATUS dstat){
//     dStatus = dstat;
//     if(dStatus == RES_OK)
//     {
//       disk.is_initialized[pdrvHolder] = 1;
//     }
//     callbackHolder();
//   });
//   }
//   // return stat;
// }

/**
  * @brief  Reads Sector(s)
  * @param  pdrv: Physical drive number (0..)
  * @param  *buff: Data buffer to store read data
  * @param  sector: Sector address (LBA)
  * @param  count: Number of sectors to read (1..128)
  * @retval DRESULT: Operation result
  */
void disk_read (
	BYTE pdrv,		/* Physical drive nmuber to identify the drive */
	BYTE *buff,		/* Data buffer to store read data */
	DWORD sector,	        /* Sector address in LBA */
	UINT count,		/* Number of sectors to read */
  void(*callback)(DRESULT)
)
{
  // callbackHolder = callback;
  // DRESULT res;

 disk.drv[pdrv]->disk_read(buff, sector, count,callback);
  // [](DRESULT res){
  // dResult = res;
  // callbackHolder();
  // });
  // dResult =  res;
}

/**
  * @brief  Writes Sector(s)
  * @param  pdrv: Physical drive number (0..)
  * @param  *buff: Data to be written
  * @param  sector: Sector address (LBA)
  * @param  count: Number of sectors to write (1..128)
  * @retval DRESULT: Operation result
  */
#if _NB_USE_WRITE == 1
void disk_write (
	BYTE pdrv,		/* Physical drive nmuber to identify the drive */
	const BYTE *buff,	/* Data to be written */
	DWORD sector,		/* Sector address in LBA */
	UINT count,        	/* Number of sectors to write */
  void(*callback)(DRESULT)
)
{
  // callbackHolder = callback;

  disk.drv[pdrv]->disk_write(buff, sector, count, callback);
  // [](DRESULT res){
  // dResult = res;
  // callbackHolder();
  // });
}
#endif /* _USE_WRITE == 1 */

/**
  * @brief  I/O control operation
  * @param  pdrv: Physical drive number (0..)
  * @param  cmd: Control code
  * @param  *buff: Buffer to send/receive control data
  * @retval DRESULT: Operation result
  */
#if _NB_USE_IOCTL == 1
DRESULT disk_ioctl (
	BYTE pdrv,		/* Physical drive nmuber (0..) */
	BYTE cmd,		/* Control code */
	void *buff		/* Buffer to send/receive control data */
)
{
  return disk.drv[pdrv]->disk_ioctl(cmd, buff);
}
#endif /* _USE_IOCTL == 1 */

/**
  * @brief  Gets Time from RTC
  * @param  None
  * @retval Time in DWORD
  */
__weak DWORD get_fattime (void)
{
  return 0;
}

/************************ (C) COPYRIGHT STMicroelectronics *****END OF FILE****/

