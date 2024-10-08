

/* Includes ------------------------------------------------------------------*/
#include "nb_sd_diskio.h"
#include "nb_diskio.h"
#include "nb_ffconf.h"

#include "../SDMMC.h"

#include <string.h>

#define SD_DEFAULT_BLOCK_SIZE 512

static volatile uint32_t locker = 0;

/*
 * when using cacheable memory region, it may be needed to maintain the cache
 * validity. Enable the define below to activate a cache maintenance at each
 * read and write operation.
 * Notice: This is applicable only for cortex M7 based platform.
 */

/* #define ENABLE_SD_DMA_CACHE_MAINTENANCE  1 */

/*
 * Some DMA requires 4-Byte aligned address buffer to correctly read/write data,
 * in FatFs some accesses aren't thus we need a 4-byte aligned scratch buffer to correctly
 * transfer data
 */

/* #define ENABLE_SCRATCH_BUFFER */

#if defined(ENABLE_SCRATCH_BUFFER)
#error // THIS PORTION HAS NOT BEEN TESTED OR ADJUSTED FOR NB_FATFS
#if defined(ENABLE_SD_DMA_CACHE_MAINTENANCE)
#error // THIS PORTION HAS NOT BEEN TESTED OR ADJUSTED FOR NB_FATFS
ALIGN_32BYTES(static uint8_t scratch[BLOCKSIZE]); // 32-Byte aligned for cache maintenance
#else
__ALIGN_BEGIN static uint8_t scratch[BLOCKSIZE] __ALIGN_END;
#endif
#endif
/* Disk status */
static volatile DSTATUS Stat = STA_NOT_INITIALIZED;

static void (*dstatCallback)(DSTATUS);
static void (*dresCallback)(DRESULT);

static void SD_CheckStatus(void (*callback)(DSTATUS));
void SD_status(void (*callback)(DSTATUS));
void SD_read(BYTE *, DWORD, UINT, void (*callback)(DRESULT));
#if _NB_USE_WRITE == 1
void SD_write(const BYTE *, DWORD, UINT, void (*callback)(DRESULT));
#endif /* _USE_WRITE == 1 */
#if _NB_USE_IOCTL == 1
DRESULT SD_ioctl(BYTE, void *);
#endif /* _USE_IOCTL == 1 */

const Diskio_drvTypeDef NB_SD_Driver =
    {
        SD_status,
        SD_read,
#if _NB_USE_WRITE == 1
        SD_write,
#endif /* _USE_WRITE == 1 */

#if _NB_USE_IOCTL == 1
        SD_ioctl,
#endif /* _USE_IOCTL == 1 */
};

static void SD_CheckStatus(void (*callback)(DSTATUS))
{
    // This portion just for error checking. Will eventually be removed in future update.
    if(locker){
        while(1){
            // Error
        }
    }
    locker = locker +1;

    // This portion is important to leave as is
    dstatCallback = callback;
    Stat = STA_NOT_INITIALIZED;    
    //

    // Setup IO function in this portion. Function must callback the callback function when done with IO function and pass the correct variable in order to work.
    // Non Working Example provided.

    SDMMC::sdmmc1->setCommandResponseReceivedCallback(
    []()
    {
        // This portion just for error checking. Will eventually be removed in future update.
        if(!locker){
            while(1){
                // Error
            }
        }
        locker = locker -1;

        // Disable the callbacks
        SDMMC::sdmmc1->disableCommandResponseTimeoutCallback();
        SDMMC::sdmmc1->disableCommandResponseReceivedCallback();
        // Clear the callbacks so no erronous callbacks are possible
        SDMMC::sdmmc1->setCommandResponseReceivedCallback([](){});
        SDMMC::sdmmc1->setCommandResponseTimeoutCallback([](){});

        uint32_t cardState = ((SDMMC::sdmmc1->getResponse1() >> 9U) & 0x0FU);
        if(cardState == 0x00000004U){ // Card in transfer state
            Stat &= ~STA_NOT_INITIALIZED;
        }

        /////////////////////////////////////////////////////////    IMPORTANT   ////////////////////////////////////////////////////////////////////
        // Must call this callback
        dstatCallback(Stat); 
    });

    SDMMC::sdmmc1->setCommandResponseTimeoutCallback([](){
        
        // This portion just for error checking. Will eventually be removed in future update.
        if(!locker){
            while(1){
                // Error
            }
        }
        locker = locker -1;

        // Disable the callbacks
        SDMMC::sdmmc1->disableCommandResponseTimeoutCallback();
        SDMMC::sdmmc1->disableCommandResponseReceivedCallback();
        // Clear the callbacks so no erronous callbacks are possible
        SDMMC::sdmmc1->setCommandResponseReceivedCallback([](){});
        SDMMC::sdmmc1->setCommandResponseTimeoutCallback([](){});

        /////////////////////////////////////////////////////////    IMPORTANT   ////////////////////////////////////////////////////////////////////
        // Must call this callback
        dstatCallback(Stat); 
    });

    // Enable callback
    SDMMC::sdmmc1->enableCommandResponseTimeoutCallback();
    SDMMC::sdmmc1->enableCommandResponseReceivedCallback();
    
    // Perform IO function
    SDMMC::sdmmc1->getCardState();
}

/**
 * @brief  Gets Disk Status
 * @retval DSTATUS: Operation status
 */
void SD_status(void (*callback)(DSTATUS))
{
    SD_CheckStatus(callback);
}

/**
 * @brief  Reads Sector(s)
 * @param  *buff: Data buffer to store read data
 * @param  sector: Sector address (LBA)
 * @param  count: Number of sectors to read (1..128)
 * @retval DRESULT: Operation result
 */
void SD_read(BYTE *buff, DWORD sector, UINT count, void (*callback)(DRESULT))
{
    // This portion just for error checking. Will eventually be removed in future update.
    if(locker){
        while(1){
            // Error
        }
    }
    locker = locker +1;
    dresCallback = callback;

#if (ENABLE_SD_DMA_CACHE_MAINTENANCE == 1)
#error // THIS PORTION HAS NOT BEEN TESTED OR ADJUSTED FOR NB_FATFS
    uint32_t alignedAddr;
#endif

    // Setup IO function in this portion. Function must callback the callback function when done with IO function and pass the correct variable in order to work
    // Non Working Example provided.

            
    SDMMC::sdmmc1->setDataTransferEndedCallback([](){
        
        // This portion just for error checking. Will eventually be removed in future update.
        if(!locker){
            while(1){
                // Error
            }
        }
            locker = locker -1;

        // Disable callback
        SDMMC::sdmmc1->disableDataTransferEndedCallback();
        SDMMC::sdmmc1->setDataTransferEndedCallback([](){});

        /////////////////////////////////////////////////////////    IMPORTANT   ////////////////////////////////////////////////////////////////////
        // Must call this callback
        dresCallback(RES_OK);
    });

    // Enable callback
    SDMMC::sdmmc1->enableDataTransferEndedCallback();

    // Perform IO function
    SDMMC::sdmmc1->readBlock((uint32_t*)buff, count, (uint32_t)sector);

#if (ENABLE_SD_DMA_CACHE_MAINTENANCE == 1)
#error // THIS PORTION HAS NOT BEEN TESTED OR ADJUSTED FOR NB_FATFS
            /*
            the SCB_InvalidateDCache_by_Addr() requires a 32-Byte aligned address,
            adjust the address and the D-Cache size to invalidate accordingly.
            */
            alignedAddr = (uint32_t)buff & ~0x1F;
            SCB_InvalidateDCache_by_Addr((uint32_t*)alignedAddr, count*BLOCKSIZE + ((uint32_t)buff - alignedAddr));
#endif
            
}

/* USER CODE BEGIN beforeWriteSection */
/* can be used to modify previous code / undefine following code / add new code */
/* USER CODE END beforeWriteSection */
/**
 * @brief  Writes Sector(s)
 * @param  *buff: Data to be written
 * @param  sector: Sector address (LBA)
 * @param  count: Number of sectors to write (1..128)
 * @retval DRESULT: Operation result
 */
#if _NB_USE_WRITE == 1

void SD_write(const BYTE *buff, DWORD sector, UINT count, void (*callback)(DRESULT))
{    
    // This portion just for error checking. Will eventually be removed in future update.
    if(locker){
        while(1){
            // Error
        }
    }
    locker = locker +1;

    dresCallback = callback;

#if (ENABLE_SD_DMA_CACHE_MAINTENANCE == 1)
#error // THIS PORTION HAS NOT BEEN TESTED OR ADJUSTED FOR NB_FATFS
        uint32_t alignedAddr;
        /*
        the SCB_CleanDCache_by_Addr() requires a 32-Byte aligned address
        adjust the address and the D-Cache size to clean accordingly.
        */
        alignedAddr = (uint32_t)buff & ~0x1F;
        SCB_CleanDCache_by_Addr((uint32_t *)alignedAddr, count * BLOCKSIZE + ((uint32_t)buff - alignedAddr));
#endif
        
        // Setup IO function in this portion. Function must callback the callback function when done with IO function and pass the correct variable in order to work
    // Non Working Example provided.


        SDMMC::sdmmc1->setDataTransferEndedCallback([](){
            
        // This portion just for error checking. Will eventually be removed in future update.
        if(!locker){
            while(1){
                // Error
            }
        }
            locker = locker -1;

            // Disable callback
            SDMMC::sdmmc1->disableDataTransferEndedCallback();
            SDMMC::sdmmc1->setDataTransferEndedCallback([](){});

            /////////////////////////////////////////////////////////    IMPORTANT   ////////////////////////////////////////////////////////////////////
            // Must call this callback
            dresCallback(RES_OK);
        });
        // Enable callback
        SDMMC::sdmmc1->enableDataTransferEndedCallback();

        // Perform IO function
        SDMMC::sdmmc1->writeBlock((uint32_t *)buff, count, (uint32_t)(sector));

        

}
#endif /* _USE_WRITE == 1 */

/**
 * @brief  I/O control operation
 * @param  cmd: Control code
 * @param  *buff: Buffer to send/receive control data
 * @retval DRESULT: Operation result
 */
#if _NB_USE_IOCTL == 1
DRESULT SD_ioctl(BYTE cmd, void *buff)
{
    // Default to RES_ERROR
    DRESULT res = RES_ERROR;
    

    /////////////////////////////////////////////////////////    IMPORTANT   ////////////////////////////////////////////////////////////////////
    // Set up a function to return the following results. ioctl was previously an IO function, but was changed to be synchronous, accessing pre fetched data.
    // The expectation is that the user will have the data needed for ioctl ready ahead of NB_FatFS function calls.

    if(SDMMC::sdmmc1->cardData.cardStatus != SDMMC::CardStatus::Ready)
    {
        return RES_NOT_READY;
    }

    switch (cmd)
    {
    /* Make sure that no pending write process */
    case CTRL_SYNC:
        res = RES_OK;
        break;

    /* Get number of sectors on the disk (DWORD) */
    case GET_SECTOR_COUNT:
        // BSP_SD_GetCardInfo(&CardInfo);
        *(DWORD *)buff = SDMMC::sdmmc1->cardData.CardLogicalCapacityInBlocks;
        res = RES_OK;
        break;

    /* Get R/W sector size (WORD) */
    case GET_SECTOR_SIZE:
        *(WORD *)buff = SDMMC::sdmmc1->cardData.LogicalBlockSize;
        res = RES_OK;
        break;

    /* Get erase block size in unit of sector (DWORD) */
    case GET_BLOCK_SIZE:
        *(DWORD *)buff = SDMMC::sdmmc1->cardData.LogicalBlockSize / SD_DEFAULT_BLOCK_SIZE;
        res = RES_OK;
        break;

    default:
        res = RES_INVALID_PARAMETER;
    }

    return res;
}
#endif /* _USE_IOCTL == 1 */
