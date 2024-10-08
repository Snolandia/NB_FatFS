/**
  ******************************************************************************
  * @file    ff_gen_drv.h
  * @author  MCD Application Team
  * @brief   Header for ff_gen_drv.c module.
  *****************************************************************************
  * @attention
  *
  * Copyright (c) 2017 STMicroelectronics. All rights reserved.
  *
  * This software component is licensed by ST under BSD 3-Clause license,
  * the "License"; You may not use this file except in compliance with the
  * License. You may obtain a copy of the License at:
  *                       opensource.org/licenses/BSD-3-Clause
  *
  ******************************************************************************
**/

/* Define to prevent recursive inclusion -------------------------------------*/
#ifndef __NB_FF_GEN_DRV_H
#define __NB_FF_GEN_DRV_H

// #ifdef __cplusplus
//  extern "C" {
// #endif

/* Includes ------------------------------------------------------------------*/
#include "nb_diskio.h"
#include "nb_ff.h"
#include "stdint.h"


/* Exported types ------------------------------------------------------------*/

/**
  * @brief  Disk IO Driver structure definition
  */
typedef struct
{
  void (*disk_status)     (void(*callback)(DSTATUS));                     /*!< Get Disk Status                           */
  void (*disk_read)       (BYTE*, DWORD, UINT, void(*callback)(DRESULT));       /*!< Read Sector(s)                            */
#if _NB_USE_WRITE == 1
  void (*disk_write)      (const BYTE*, DWORD, UINT, void(*callback)(DRESULT)); /*!< Write Sector(s) when _USE_WRITE = 0       */
#endif /* _USE_WRITE == 1 */
#if _NB_USE_IOCTL == 1
  DRESULT (*disk_ioctl)      (BYTE, void*);              /*!< I/O control operation when _USE_IOCTL = 1 */
#endif /* _USE_IOCTL == 1 */

}Diskio_drvTypeDef;

/**
  * @brief  Global Disk IO Drivers structure definition
  */
typedef struct
{
  uint8_t                 is_initialized[_NB_VOLUMES];
  const Diskio_drvTypeDef *drv[_NB_VOLUMES];
  uint8_t                 lun[_NB_VOLUMES];
  volatile uint8_t        nbr;

}Disk_drvTypeDef;

/* Exported constants --------------------------------------------------------*/
/* Exported macro ------------------------------------------------------------*/
/* Exported functions ------------------------------------------------------- */
uint8_t NB_FATFS_LinkDriver(const Diskio_drvTypeDef *drv, char *path);
uint8_t NB_FATFS_UnLinkDriver(char *path);
uint8_t NB_FATFS_LinkDriverEx(const Diskio_drvTypeDef *drv, char *path, BYTE lun);
uint8_t NB_FATFS_UnLinkDriverEx(char *path, BYTE lun);
uint8_t NB_FATFS_GetAttachedDriversNbr(void);

// #ifdef __cplusplus
// }
// #endif

#endif /* __FF_GEN_DRV_H */

/************************ (C) COPYRIGHT STMicroelectronics *****END OF FILE****/

