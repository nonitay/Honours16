#####
#10-03-16- Real data
#Combining NA dataframes
####

library(igraph)
library(network)
library(entropy)

#############################################################################

#ADELNA01 <- rbind (ADEL01_G.netMx, ADEL01_B.netMx, ADEL01_SF.netMx, ADEL01_TF.netMx,ADEL01_SAM.netMx, ADEL01_TAM.netMx, ADEL01_SDM.netMx, ADEL01_TDM.netMx,ADEL01_SD.netMx, ADEL01_TD.netMx, ADEL01_QT.netMx)
WCE01_G.netMx
WCE01_B.netMx
WCE01_SF.netMx
WCE01_TF.netMx
WCE01_SAM.netMx
WCE01_TAM.netMx
WCE01_SDM.netMx
WCE01_TDM.netMx
WCE01_SD.netMx
WCE01_TD.netMx 
WCE01_QT.netMx

#############################################################################
# ROUND 1 
#############################################################################
ADELNA01 <- rbind (ADEL01_G.netMx, ADEL01_B.netMx, ADEL01_TF.netMx,
                   ADEL01_TAM.netMx, ADEL01_TDM.netMx)

BLNA01 <- rbind (BL01_G.netMx,BL01_TAM.netMx, BL01_SDM.netMx, BL01_TDM.netMx)

CARLNA01 <- rbind (CARL01_TAM.netMx,CARL01_SDM.netMx, CARL01_TDM.netMx)

COLLNA01 <- rbind (COLL01_G.netMx,COLL01_TAM.netMx, COLL01_TDM.netMx)

ESSNA01 <- rbind (ESS01_TAM.netMx, ESS01_TDM.netMx, ESS01_TD.netMx)

FRENA01 <- rbind (FRE01_SF.netMx,FRE01_SAM.netMx, FRE01_SDM.netMx, FRE01_TDM.netMx)

GCFCNA01 <- rbind (GCFC01_B.netMx, GCFC01_SF.netMx, GCFC01_TAM.netMx, GCFC01_TDM.netMx, GCFC01_TD.netMx)

GEELNA01 <- rbind (GEEL01_G.netMx, GEEL01_B.netMx, GEEL01_TF.netMx, GEEL01_TAM.netMx, GEEL01_SDM.netMx, GEEL01_TDM.netMx, GEEL01_TD.netMx)

GWSNA01 <- rbind (GWS01_G.netMx, GWS01_B.netMx, GWS01_TF.netMx, GWS01_SDM.netMx, GWS01_TDM.netMx)

HAWNA01 <- rbind (HAW01_B.netMx, HAW01_TF.netMx, HAW01_TAM.netMx, HAW01_TDM.netMx, HAW01_TD.netMx)

MELBNA01 <- rbind (MELB01_G.netMx, MELB01_TF.netMx, MELB01_TAM.netMx, MELB01_SDM.netMx, MELB01_TDM.netMx)

NMFCNA01 <- rbind (NMFC01_G.netMx, NMFC01_TAM.netMx, NMFC01_SDM.netMx, NMFC01_TDM.netMx)

PORTNA01 <- rbind (PORT01_G.netMx, PORT01_SF.netMx, PORT01_TF.netMx,PORT01_SAM.netMx, PORT01_TAM.netMx, PORT01_TDM.netMx)

RICHNA01 <- rbind (RICH01_TF.netMx,RICH01_SAM.netMx, RICH01_TAM.netMx, RICH01_SDM.netMx, RICH01_TDM.netMx)

STKNA01 <- rbind (STK01_SF.netMx,STK01_SAM.netMx, STK01_TAM.netMx, STK01_SDM.netMx, STK01_TDM.netMx, STK01_TD.netMx)

SYDNA01 <- rbind (SYD01_SF.netMx, SYD01_TF.netMx, SYD01_TAM.netMx)

WBNA01 <- rbind (WB01_TDM.netMx)

WCENA01 <- rbind (WCE01_G.netMx, WCE01_SF.netMx, WCE01_TF.netMx, WCE01_TAM.netMx, WCE01_SDM.netMx, WCE01_TDM.netMx)

#Combine all round 1 data
RD1_na <- rbind (ADELNA01, BLNA01, CARLNA01, COLLNA01, ESSNA01, FRENA01, GCFCNA01, GEELNA01, GWSNA01, HAWNA01, MELBNA01, NMFCNA01, PORTNA01, RICHNA01, STKNA01, SYDNA01, WBNA01, WCENA01)

#############################################################################
# ROUND 2
#############################################################################

#ADELNA02 <- rbind (ADEL02_G.netMx, ADEL02_B.netMx, ADEL02_SF.netMx, ADEL02_TF.netMx,ADEL02_SAM.netMx, ADEL02_TAM.netMx, ADEL02_SDM.netMx, ADEL02_TDM.netMx,ADEL02_SD.netMx, ADEL02_TD.netMx, ADEL02_QT.netMx)
WCE02_G.netMx
WCE02_B.netMx
WCE02_SF.netMx
WCE02_TF.netMx
WCE02_SAM.netMx
WCE02_TAM.netMx
WCE02_SDM.netMx
WCE02_TDM.netMx
WCE02_SD.netMx
WCE02_TD.netMx 
WCE02_QT.netMx

ADELNA02 <- rbind (ADEL02_G.netMx, ADEL02_TF.netMx, ADEL02_TAM.netMx, ADEL02_SDM.netMx, ADEL02_TDM.netMx)

BLNA02 <- rbind (BL02_B.netMx, BL02_SF.netMx, BL02_TF.netMx,BL02_SAM.netMx, BL02_TAM.netMx, BL02_SDM.netMx, BL02_TDM.netMx)

CARLNA02 <- rbind (CARL02_SF.netMx,CARL02_SAM.netMx, CARL02_SDM.netMx, CARL02_TDM.netMx,CARL02_SD.netMx)

COLLNA02 <- rbind (COLL02_G.netMx, COLL02_SF.netMx, COLL02_TF.netMx,COLL02_SAM.netMx, COLL02_TAM.netMx, COLL02_SDM.netMx, COLL02_TDM.netMx, COLL02_QT.netMx)

ESSNA02 <- rbind (ESS02_SF.netMx, ESS02_TF.netMx, ESS02_SDM.netMx, ESS02_TDM.netMx, ESS02_TD.netMx, ESS02_QT.netMx)

FRENA02 <- rbind (FRE02_TF.netMx, FRE02_TAM.netMx, FRE02_SDM.netMx, FRE02_TDM.netMx)

GCFCNA02 <- rbind (GCFC02_SF.netMx, GCFC02_TF.netMx, GCFC02_TAM.netMx)

GEELNA02 <- rbind (GEEL02_TF.netMx, GEEL02_TAM.netMx, GEEL02_SDM.netMx, GEEL02_TDM.netMx)

GWSNA02 <- rbind (GWS02_SF.netMx, GWS02_TF.netMx,GWS02_SAM.netMx)

HAWNA02 <- rbind (HAW02_TF.netMx, HAW02_TAM.netMx, HAW02_SDM.netMx)

MELBNA02 <- rbind (MELB02_TAM.netMx, MELB02_SDM.netMx, MELB02_TDM.netMx)

NMFCNA02 <- rbind (NMFC02_G.netMx, NMFC02_SF.netMx, NMFC02_TF.netMx,NMFC02_SDM.netMx, NMFC02_TDM.netMx, NMFC02_TD.netMx)

PORTNA02 <- rbind (PORT02_TF.netMx,PORT02_SAM.netMx, PORT02_TAM.netMx, PORT02_TDM.netMx,PORT02_SD.netMx)

RICHNA02 <- rbind (RICH02_B.netMx,RICH02_TF.netMx, RICH02_SDM.netMx, RICH02_TDM.netMx)

STKNA02 <- rbind (STK02_G.netMx, STK02_TF.netMx,STK02_SAM.netMx, STK02_TAM.netMx, STK02_SDM.netMx, STK02_TDM.netMx)

SYDNA02 <- rbind (SYD02_G.netMx,SYD02_SAM.netMx, SYD02_TAM.netMx, SYD02_TDM.netMx)

WBNA02 <- rbind (WB02_SAM.netMx, WB02_TAM.netMx, WB02_TDM.netMx, WB02_TD.netMx)

WCENA02 <- rbind (WCE02_G.netMx, WCE02_SF.netMx,WCE02_TDM.netMx)

#Combine all round 2 data
RD2_na <- rbind (ADELNA02, BLNA02, CARLNA02, COLLNA02, ESSNA02, FRENA02, GCFCNA02, GEELNA02, GWSNA02, HAWNA02, MELBNA02, NMFCNA02, PORTNA02, RICHNA02, STKNA02, SYDNA02, WBNA02, WCENA02)

#############################################################################
# ROUND 3
#############################################################################

#ADELNA03 <- rbind (ADEL03_G.netMx, ADEL03_B.netMx, ADEL03_SF.netMx, ADEL03_TF.netMx,ADEL03_SAM.netMx, ADEL03_TAM.netMx, ADEL03_SDM.netMx, ADEL03_TDM.netMx,ADEL03_SD.netMx, ADEL03_TD.netMx, ADEL03_QT.netMx)
WCE03_G.netMx
WCE03_B.netMx
WCE03_SF.netMx
WCE03_TF.netMx
WCE03_SAM.netMx
WCE03_TAM.netMx
WCE03_SDM.netMx
WCE03_TDM.netMx
WCE03_SD.netMx
WCE03_TD.netMx 
WCE03_QT.netMx

ADELNA03 <- rbind (ADEL03_TF.netMx,ADEL03_SAM.netMx, ADEL03_SDM.netMx, ADEL03_TDM.netMx, ADEL03_TD.netMx)

BLNA03 <- rbind (BL03_TF.netMx,BL03_SAM.netMx, BL03_TDM.netMx, BL03_TD.netMx)

CARLNA03 <- rbind (CARL03_SF.netMx, CARL03_TF.netMx, CARL03_TAM.netMx, CARL03_TDM.netMx, CARL03_QT.netMx)

COLLNA03 <- rbind (COLL03_TF.netMx,COLL03_SAM.netMx, COLL03_TAM.netMx)

ESSNA03 <- rbind (ESS03_G.netMx, ESS03_SF.netMx, ESS03_TF.netMx,ESS03_SAM.netMx, ESS03_TAM.netMx,ESS03_TDM.netMx,ESS03_SD.netMx, ESS03_TD.netMx)

FRENA03 <- rbind (FRE03_G.netMx,FRE03_SAM.netMx, FRE03_TAM.netMx, FRE03_TDM.netMx)

GCFCNA03 <- rbind (GCFC03_G.netMx, GCFC03_TF.netMx, GCFC03_TAM.netMx, GCFC03_TDM.netMx)

GEELNA03 <- rbind (GEEL03_G.netMx, GEEL03_SF.netMx,GEEL03_SAM.netMx, GEEL03_TAM.netMx, GEEL03_TDM.netMx)

GWSNA03 <- rbind (GWS03_G.netMx, GWS03_B.netMx, GWS03_TF.netMx, GWS03_TAM.netMx, GWS03_SDM.netMx, GWS03_TDM.netMx, GWS03_QT.netMx)

HAWNA03 <- rbind (HAW03_G.netMx, HAW03_B.netMx, HAW03_SF.netMx, HAW03_TAM.netMx, HAW03_SDM.netMx, HAW03_TDM.netMx)

MELBNA03 <- rbind (MELB03_B.netMx, MELB03_SDM.netMx)

NMFCNA03 <- rbind (NMFC03_G.netMx, NMFC03_B.netMx, NMFC03_TAM.netMx, NMFC03_SDM.netMx, NMFC03_TDM.netMx)

PORTNA03 <- rbind (PORT03_G.netMx, PORT03_B.netMx, PORT03_SF.netMx, PORT03_TF.netMx, PORT03_TAM.netMx, PORT03_SDM.netMx)

RICHNA03 <- rbind (RICH03_G.netMx, RICH03_SF.netMx, RICH03_TF.netMx, RICH03_TAM.netMx)

STKNA03 <- rbind (STK03_G.netMx,STK03_SAM.netMx, STK03_SDM.netMx, STK03_TDM.netMx)

SYDNA03 <- rbind (SYD03_G.netMx, SYD03_B.netMx, SYD03_TF.netMx,SYD03_SAM.netMx, SYD03_TAM.netMx, SYD03_SDM.netMx, SYD03_TDM.netMx, SYD03_TD.netMx)

WBNA03 <- rbind (WB03_SF.netMx, WB03_TF.netMx, WB03_TAM.netMx, WB03_SDM.netMx, WB03_TDM.netMx,WB03_SD.netMx)

WCENA03 <- rbind (WCE03_G.netMx, WCE03_TF.netMx, WCE03_TAM.netMx, WCE03_TDM.netMx)

#Combine all round 3 data
RD3_na <- rbind (ADELNA03, BLNA03, CARLNA03, COLLNA03, ESSNA03, FRENA03, GCFCNA03, GEELNA03, GWSNA03, HAWNA03, MELBNA03, NMFCNA03, PORTNA03, RICHNA03, STKNA03, SYDNA03, WBNA03, WCENA03)

#############################################################################
# ROUND 4
#############################################################################

#ADELNA04 <- rbind (ADEL04_G.netMx, ADEL04_B.netMx, ADEL04_SF.netMx, ADEL04_TF.netMx,ADEL04_SAM.netMx, ADEL04_TAM.netMx, ADEL04_SDM.netMx, ADEL04_TDM.netMx,ADEL04_SD.netMx, ADEL04_TD.netMx, ADEL04_QT.netMx)
WCE04_G.netMx
WCE04_B.netMx
WCE04_SF.netMx
WCE04_TF.netMx
WCE04_SAM.netMx
WCE04_TAM.netMx
WCE04_SDM.netMx
WCE04_TDM.netMx
WCE04_SD.netMx
WCE04_TD.netMx 
WCE04_QT.netMx

ADELNA04 <- rbind (ADEL04_G.netMx, ADEL04_SF.netMx, ADEL04_TF.netMx, ADEL04_TAM.netMx, ADEL04_TDM.netMx, ADEL04_QT.netMx)

BLNA04 <- rbind (BL04_B.netMx,BL04_SDM.netMx, BL04_TDM.netMx)

CARLNA04 <- rbind (CARL04_TF.netMx, CARL04_TAM.netMx, CARL04_SDM.netMx, CARL04_TDM.netMx)

COLLNA04 <- rbind (COLL04_B.netMx, COLL04_SF.netMx, COLL04_TF.netMx, COLL04_TAM.netMx, COLL04_TDM.netMx)

ESSNA04 <- rbind (ESS04_TF.netMx, ESS04_TAM.netMx, ESS04_SDM.netMx, ESS04_TDM.netMx)

FRENA04 <- rbind (FRE04_SF.netMx, FRE04_TF.netMx,FRE04_SAM.netMx, FRE04_TAM.netMx, FRE04_TDM.netMx)

GCFCNA04 <- rbind (GCFC04_B.netMx, GCFC04_TF.netMx, GCFC04_TAM.netMx, GCFC04_TDM.netMx, GCFC04_TD.netMx)

GEELNA04 <- rbind (GEEL04_G.netMx, GEEL04_B.netMx, GEEL04_SF.netMx, GEEL04_TF.netMx, GEEL04_SDM.netMx, GEEL04_TDM.netMx, GEEL04_TD.netMx)

GWSNA04 <- rbind (GWS04_G.netMx, GWS04_B.netMx, GWS04_SF.netMx, GWS04_TF.netMx, GWS04_TAM.netMx, GWS04_SDM.netMx)

HAWNA04 <- rbind (HAW04_TF.netMx,HAW04_SAM.netMx, HAW04_TDM.netMx)

MELBNA04 <- rbind (MELB04_SAM.netMx, MELB04_TAM.netMx, MELB04_TDM.netMx, MELB04_TD.netMx)

NMFCNA04 <- rbind (NMFC04_G.netMx, NMFC04_TF.netMx, NMFC04_TAM.netMx, NMFC04_TDM.netMx, NMFC04_QT.netMx)

PORTNA04 <- rbind (PORT04_G.netMx, PORT04_SDM.netMx, PORT04_TDM.netMx)

RICHNA04 <- rbind (RICH04_B.netMx, RICH04_TF.netMx, RICH04_TAM.netMx, RICH04_TDM.netMx, RICH04_TD.netMx)

STKNA04 <- rbind (STK04_TF.netMx, STK04_TAM.netMx, STK04_SDM.netMx, STK04_TDM.netMx, STK04_TD.netMx)

SYDNA04 <- rbind (SYD04_B.netMx, SYD04_TF.netMx, SYD04_TAM.netMx, SYD04_SDM.netMx, SYD04_TDM.netMx)

WBNA04 <- rbind (WB04_TF.netMx, WB04_TAM.netMx, WB04_SDM.netMx, WB04_TDM.netMx)

WCENA04 <- rbind (WCE04_G.netMx,WCE04_TF.netMx, WCE04_TAM.netMx, WCE04_SDM.netMx, WCE04_TDM.netMx)

#Combine all round 4 data
RD4_na <- rbind (ADELNA04, BLNA04, CARLNA04, COLLNA04, ESSNA04, FRENA04, GCFCNA04, GEELNA04, GWSNA04, HAWNA04, MELBNA04, NMFCNA04, PORTNA04, RICHNA04, STKNA04, SYDNA04, WBNA04, WCENA04)

#############################################################################
# ROUND 5
#############################################################################

#ADELNA05 <- rbind (ADEL05_G.netMx, ADEL05_B.netMx, ADEL05_SF.netMx, ADEL05_TF.netMx,ADEL05_SAM.netMx, ADEL05_TAM.netMx, ADEL05_SDM.netMx, ADEL05_TDM.netMx,ADEL05_SD.netMx, ADEL05_TD.netMx, ADEL05_QT.netMx)
WCE05_G.netMx
WCE05_B.netMx
WCE05_SF.netMx
WCE05_TF.netMx
WCE05_SAM.netMx
WCE05_TAM.netMx
WCE05_SDM.netMx
WCE05_TDM.netMx
WCE05_SD.netMx
WCE05_TD.netMx 
WCE05_QT.netMx

ADELNA05 <- rbind (ADEL05_B.netMx, ADEL05_TF.netMx,ADEL05_SAM.netMx, ADEL05_TAM.netMx, ADEL05_SDM.netMx, ADEL05_TDM.netMx)

BLNA05 <- rbind (BL05_G.netMx, BL05_B.netMx, BL05_SF.netMx, BL05_TAM.netMx, BL05_SDM.netMx, BL05_TDM.netMx) 

CARLNA05 <- rbind (CARL05_SAM.netMx, CARL05_SDM.netMx, CARL05_TDM.netMx)

COLLNA05 <- rbind (COLL05_G.netMx,COLL05_TF.netMx, COLL05_TAM.netMx, COLL05_SDM.netMx, COLL05_TDM.netMx)

ESSNA05 <- rbind (ESS05_G.netMx,ESS05_SAM.netMx, ESS05_TAM.netMx, ESS05_SDM.netMx, ESS05_TDM.netMx, ESS05_TD.netMx)

FRENA05 <- rbind (FRE05_G.netMx, FRE05_B.netMx, FRE05_TF.netMx,FRE05_SAM.netMx, FRE05_TAM.netMx, FRE05_TDM.netMx, FRE05_QT.netMx)

GCFCNA05 <- rbind (GCFC05_SF.netMx, GCFC05_TF.netMx, GCFC05_TAM.netMx, GCFC05_TDM.netMx, GCFC05_TD.netMx)

GEELNA05 <- rbind (GEEL05_G.netMx, GEEL05_SF.netMx, GEEL05_TF.netMx,GEEL05_SAM.netMx, GEEL05_TAM.netMx, GEEL05_SDM.netMx, GEEL05_TDM.netMx)

GWSNA05 <- rbind (GWS05_G.netMx, GWS05_B.netMx,GWS05_SAM.netMx, GWS05_TAM.netMx, GWS05_SDM.netMx, GWS05_TDM.netMx)

HAWNA05 <- rbind (HAW05_G.netMx, HAW05_B.netMx, HAW05_TF.netMx,HAW05_SAM.netMx, HAW05_TAM.netMx, HAW05_SDM.netMx)

MELBNA05 <- rbind (MELB05_SF.netMx,MELB05_SAM.netMx, MELB05_TAM.netMx, MELB05_SDM.netMx, MELB05_TDM.netMx)

NMFCNA05 <- rbind (NMFC05_G.netMx, NMFC05_B.netMx, NMFC05_TF.netMx, NMFC05_TAM.netMx, NMFC05_SDM.netMx, NMFC05_TDM.netMx,NMFC05_SD.netMx)

PORTNA05 <- rbind (PORT05_TF.netMx, PORT05_TAM.netMx, PORT05_SDM.netMx)

RICHNA05 <- rbind (RICH05_G.netMx, RICH05_B.netMx, RICH05_SF.netMx, RICH05_TF.netMx,RICH05_SAM.netMx, RICH05_TAM.netMx, RICH05_SDM.netMx, RICH05_TDM.netMx)

STKNA05 <- rbind (STK05_G.netMx, STK05_B.netMx,STK05_SAM.netMx, STK05_TAM.netMx, STK05_TDM.netMx)

#SYDNA05 

WBNA05 <- rbind (WB05_SF.netMx, WB05_SDM.netMx, WB05_TDM.netMx)

WCENA05 <- rbind (WCE05_G.netMx, WCE05_B.netMx, WCE05_TAM.netMx, WCE05_SDM.netMx, WCE05_TDM.netMx)

#Combine all round 5 data
RD5_na <- rbind (ADELNA05, BLNA05, CARLNA05, COLLNA05, ESSNA05, FRENA05, GCFCNA05, GEELNA05, GWSNA05, HAWNA05, MELBNA05, NMFCNA05, PORTNA05, RICHNA05, STKNA05, WBNA05, WCENA05)

#############################################################################
# ROUND 6
#############################################################################

#ADELNA06 <- rbind (ADEL06_G.netMx, ADEL06_B.netMx, ADEL06_SF.netMx, ADEL06_TF.netMx,ADEL06_SAM.netMx, ADEL06_TAM.netMx, ADEL06_SDM.netMx, ADEL06_TDM.netMx,ADEL06_SD.netMx, ADEL06_TD.netMx, ADEL06_QT.netMx)
WCE06_G.netMx
WCE06_B.netMx
WCE06_SF.netMx
WCE06_TF.netMx
WCE06_SAM.netMx
WCE06_TAM.netMx
WCE06_SDM.netMx
WCE06_TDM.netMx
WCE06_SD.netMx
WCE06_TD.netMx 
WCE06_QT.netMx

ADELNA06 <- rbind (ADEL06_G.netMx, ADEL06_TF.netMx, ADEL06_TAM.netMx, ADEL06_TDM.netMx, ADEL06_TD.netMx)

BLNA06 <- rbind (BL06_SAM.netMx, BL06_TDM.netMx)

CARLNA06 <- rbind (CARL06_TF.netMx,CARL06_SAM.netMx, CARL06_SDM.netMx, CARL06_TDM.netMx, CARL06_TD.netMx)

COLLNA06 <- rbind (COLL06_G.netMx, COLL06_B.netMx, COLL06_SF.netMx, COLL06_TF.netMx, COLL06_SDM.netMx, COLL06_TDM.netMx)

ESSNA06 <- rbind (ESS06_TAM.netMx, ESS06_TDM.netMx)

FRENA06 <- rbind (FRE06_TF.netMx, FRE06_TAM.netMx, FRE06_SDM.netMx, FRE06_TDM.netMx)

GCFCNA06 <- rbind (GCFC06_G.netMx, GCFC06_B.netMx, GCFC06_SF.netMx, GCFC06_TF.netMx, GCFC06_SDM.netMx, GCFC06_TDM.netMx,GCFC06_SD.netMx)

GEELNA06 <- rbind (GEEL06_G.netMx, GEEL06_B.netMx, GEEL06_TF.netMx, GEEL06_TAM.netMx, GEEL06_TDM.netMx, GEEL06_QT.netMx)

GWSNA06 <- rbind (GWS06_B.netMx, GWS06_TDM.netMx)

HAWNA06 <- rbind (HAW06_TAM.netMx, HAW06_TDM.netMx,HAW06_SD.netMx, HAW06_TD.netMx)

MELBNA06 <- rbind (MELB06_B.netMx, MELB06_TF.netMx,MELB06_SAM.netMx, MELB06_TAM.netMx, MELB06_SDM.netMx, MELB06_TDM.netMx, MELB06_TD.netMx)

NMFCNA06 <- rbind (NMFC06_B.netMx, NMFC06_TF.netMx,NMFC06_SAM.netMx, NMFC06_TAM.netMx, NMFC06_TDM.netMx)

PORTNA06 <- rbind (PORT06_G.netMx, PORT06_TF.netMx, PORT06_TDM.netMx, PORT06_TD.netMx)

RICHNA06 <- rbind (RICH06_G.netMx, RICH06_B.netMx, RICH06_TF.netMx, RICH06_TAM.netMx, RICH06_TDM.netMx)

STKNA06 <- rbind (STK06_B.netMx, STK06_SDM.netMx, STK06_TDM.netMx)

SYDNA06 <- rbind (SYD06_B.netMx, SYD06_SF.netMx, SYD06_TF.netMx, SYD06_TAM.netMx, SYD06_SDM.netMx)

WBNA06 <- rbind (WB06_B.netMx, WB06_TF.netMx,WB06_SAM.netMx, WB06_SDM.netMx, WB06_TDM.netMx)

WCENA06 <- rbind (WCE06_TF.netMx, WCE06_TDM.netMx)

#Combine all round 6 data
RD6_na <- rbind (ADELNA06, BLNA06, CARLNA06, COLLNA06, ESSNA06, FRENA06, GCFCNA06, GEELNA06, GWSNA06, HAWNA06, MELBNA06, NMFCNA06, PORTNA06, RICHNA06, STKNA06, SYDNA06, WBNA06, WCENA06)

#############################################################################
# ROUND 7
#############################################################################

#ADELNA07 <- rbind (ADEL07_G.netMx, ADEL07_B.netMx, ADEL07_SF.netMx, ADEL07_TF.netMx,ADEL07_SAM.netMx, ADEL07_TAM.netMx, ADEL07_SDM.netMx, ADEL07_TDM.netMx,ADEL07_SD.netMx, ADEL07_TD.netMx, ADEL07_QT.netMx)
WCE07_G.netMx
WCE07_B.netMx
WCE07_SF.netMx
WCE07_TF.netMx
WCE07_SAM.netMx
WCE07_TAM.netMx
WCE07_SDM.netMx
WCE07_TDM.netMx
WCE07_SD.netMx
WCE07_TD.netMx 
WCE07_QT.netMx

ADELNA07 <- rbind (ADEL07_G.netMx, ADEL07_B.netMx, ADEL07_SF.netMx,ADEL07_SAM.netMx,ADEL07_TDM.netMx,ADEL07_SD.netMx)

BLNA07 <- rbind (BL07_G.netMx,BL07_TF.netMx, BL07_TAM.netMx, BL07_SDM.netMx, BL07_TDM.netMx)

CARLNA07 <- rbind (CARL07_G.netMx, CARL07_TF.netMx,CARL07_SAM.netMx, CARL07_TAM.netMx, CARL07_SDM.netMx, CARL07_TDM.netMx, CARL07_TD.netMx)

COLLNA07 <- rbind (COLL07_G.netMx, COLL07_TAM.netMx, COLL07_TDM.netMx)

ESSNA07 <- rbind (ESS07_G.netMx,ESS07_TF.netMx, ESS07_TAM.netMx, ESS07_SDM.netMx, ESS07_TDM.netMx)

FRENA07 <- rbind (FRE07_B.netMx,FRE07_TAM.netMx)

GCFCNA07 <- rbind (GCFC07_SAM.netMx, GCFC07_TAM.netMx, GCFC07_SDM.netMx, GCFC07_TDM.netMx)

GEELNA07 <- rbind (GEEL07_TF.netMx,GEEL07_SAM.netMx, GEEL07_TAM.netMx, GEEL07_SDM.netMx, GEEL07_TDM.netMx)

GWSNA07 <- rbind (GWS07_SAM.netMx)

HAWNA07 <- rbind (HAW07_B.netMx, HAW07_TDM.netMx,HAW07_SD.netMx, HAW07_TD.netMx)

MELBNA07 <- rbind (MELB07_B.netMx, MELB07_TF.netMx, MELB07_TDM.netMx)

NMFCNA07 <- rbind (NMFC07_G.netMx,NMFC07_TF.netMx, NMFC07_TAM.netMx, NMFC07_SDM.netMx)

PORTNA07 <- rbind (PORT07_TF.netMx, PORT07_TAM.netMx, PORT07_SDM.netMx)

RICHNA07 <- rbind (RICH07_G.netMx,RICH07_TF.netMx, RICH07_TAM.netMx, RICH07_TDM.netMx)

STKNA07 <- rbind (STK07_B.netMx, STK07_TF.netMx,STK07_SAM.netMx, STK07_TAM.netMx, STK07_SDM.netMx, STK07_TDM.netMx,STK07_SD.netMx)

SYDNA07 <- rbind (SYD07_G.netMx, SYD07_B.netMx, SYD07_TF.netMx,SYD07_SAM.netMx, SYD07_TAM.netMx, SYD07_TDM.netMx)

WBNA07 <- rbind (WB07_G.netMx, WB07_SF.netMx, WB07_TF.netMx, WB07_TAM.netMx, WB07_TDM.netMx)

WCENA07 <- rbind (WCE07_SF.netMx,WCE07_SAM.netMx, WCE07_TAM.netMx)

#Combine all round 7 data
RD7_na <- rbind (ADELNA07, BLNA07, CARLNA07, COLLNA07, ESSNA07, FRENA07, GCFCNA07, GEELNA07, GWSNA07, HAWNA07, MELBNA07, NMFCNA07, PORTNA07, RICHNA07, STKNA07, SYDNA07, WBNA07, WCENA07)

#############################################################################
# ROUND 8
#############################################################################

#ADELNA08 <- rbind (ADEL08_G.netMx, ADEL08_B.netMx, ADEL08_SF.netMx, ADEL08_TF.netMx,ADEL08_SAM.netMx, ADEL08_TAM.netMx, ADEL08_SDM.netMx, ADEL08_TDM.netMx,ADEL08_SD.netMx, ADEL08_TD.netMx, ADEL08_QT.netMx)
WCE08_G.netMx
WCE08_B.netMx
WCE08_SF.netMx
WCE08_TF.netMx
WCE08_SAM.netMx
WCE08_TAM.netMx
WCE08_SDM.netMx
WCE08_TDM.netMx
WCE08_SD.netMx
WCE08_TD.netMx 
WCE08_QT.netMx

ADELNA08 <- rbind (ADEL08_SAM.netMx, ADEL08_TAM.netMx, ADEL08_SDM.netMx, ADEL08_TDM.netMx, ADEL08_TD.netMx)

BLNA08 <- rbind (BL08_TF.netMx, BL08_SDM.netMx, BL08_TDM.netMx)

CARLNA08 <- rbind (CARL08_G.netMx, CARL08_TF.netMx, CARL08_SDM.netMx, CARL08_TDM.netMx, CARL08_TD.netMx)

COLLNA08 <- rbind (COLL08_SAM.netMx, COLL08_TAM.netMx, COLL08_SDM.netMx, COLL08_TDM.netMx)

ESSNA08 <- rbind (ESS08_TAM.netMx)

FRENA08 <- rbind (FRE08_G.netMx, FRE08_SF.netMx,FRE08_SAM.netMx, FRE08_TAM.netMx, FRE08_TDM.netMx, FRE08_TD.netMx)

GCFCNA08 <- rbind (GCFC08_G.netMx, GCFC08_TF.netMx,GCFC08_SAM.netMx, GCFC08_TAM.netMx, GCFC08_SDM.netMx, GCFC08_TDM.netMx)

GEELNA08 <- rbind (GEEL08_G.netMx, GEEL08_TF.netMx, GEEL08_TDM.netMx)

GWSNA08 <- rbind (GWS08_SAM.netMx, GWS08_TAM.netMx, GWS08_SDM.netMx, GWS08_TDM.netMx)

HAWNA08 <- rbind (HAW08_TF.netMx, HAW08_TAM.netMx, HAW08_SDM.netMx, HAW08_TDM.netMx)

MELBNA08 <- rbind (MELB08_B.netMx, MELB08_TF.netMx, MELB08_SDM.netMx, MELB08_TDM.netMx)

NMFCNA08 <- rbind (NMFC08_B.netMx, NMFC08_TF.netMx,NMFC08_SAM.netMx, NMFC08_TDM.netMx,NMFC08_SD.netMx)

PORTNA08 <- rbind (PORT08_TF.netMx, PORT08_TAM.netMx, PORT08_SDM.netMx, PORT08_TDM.netMx, PORT08_TD.netMx)

RICHNA08 <- rbind (RICH08_G.netMx,RICH08_TF.netMx,RICH08_SAM.netMx,RICH08_TDM.netMx,RICH08_SD.netMx)

STKNA08 <- rbind (STK08_SF.netMx,STK08_SAM.netMx, STK08_TAM.netMx, STK08_TDM.netMx,STK08_SD.netMx, STK08_TD.netMx, STK08_QT.netMx)

SYDNA08 <- rbind (SYD08_SF.netMx,SYD08_SAM.netMx, SYD08_TAM.netMx, SYD08_SDM.netMx, SYD08_TDM.netMx, SYD08_TD.netMx)

WBNA08 <- rbind (WB08_TF.netMx, WB08_TAM.netMx, WB08_SDM.netMx, WB08_TDM.netMx)

WCENA08 <- rbind (WCE08_G.netMx, WCE08_SF.netMx, WCE08_TAM.netMx, WCE08_SDM.netMx)

#Combine all round 8 data
RD8_na <- rbind (ADELNA08, BLNA08, CARLNA08, COLLNA08, ESSNA08, FRENA08, GCFCNA08, GEELNA08, GWSNA08, HAWNA08, MELBNA08, NMFCNA08, PORTNA08, RICHNA08, STKNA08, SYDNA08, WBNA08, WCENA08)

#############################################################################
# ROUND 9
#############################################################################

#ADELNA09 <- rbind (ADEL09_G.netMx, ADEL09_B.netMx, ADEL09_SF.netMx, ADEL09_TF.netMx,ADEL09_SAM.netMx, ADEL09_TAM.netMx, ADEL09_SDM.netMx, ADEL09_TDM.netMx,ADEL09_SD.netMx, ADEL09_TD.netMx, ADEL09_QT.netMx)
WCE09_G.netMx
WCE09_B.netMx
WCE09_SF.netMx
WCE09_TF.netMx
WCE09_SAM.netMx
WCE09_TAM.netMx
WCE09_SDM.netMx
WCE09_TDM.netMx
WCE09_SD.netMx
WCE09_TD.netMx 
WCE09_QT.netMx

ADELNA09 <- rbind (ADEL09_SAM.netMx, ADEL09_TDM.netMx)

BLNA09 <- rbind (BL09_G.netMx, BL09_TF.netMx,BL09_SAM.netMx, BL09_TAM.netMx, BL09_SDM.netMx, BL09_TDM.netMx)

CARLNA09 <- rbind (CARL09_G.netMx, CARL09_TAM.netMx, CARL09_SDM.netMx, CARL09_TDM.netMx)

COLLNA09 <- rbind (COLL09_TF.netMx,COLL09_SAM.netMx, COLL09_TAM.netMx, COLL09_TDM.netMx)

ESSNA09 <- rbind (ESS09_G.netMx,ESS09_TAM.netMx, ESS09_TDM.netMx)

FRENA09 <- rbind (FRE09_G.netMx, FRE09_TF.netMx, FRE09_TAM.netMx, FRE09_SDM.netMx, FRE09_TDM.netMx,FRE09_SD.netMx)

GCFCNA09 <- rbind (GCFC09_G.netMx, GCFC09_TAM.netMx, GCFC09_SDM.netMx, GCFC09_TDM.netMx)

GEELNA09 <- rbind (GEEL09_G.netMx, GEEL09_B.netMx, GEEL09_TF.netMx, GEEL09_TAM.netMx, GEEL09_SDM.netMx, GEEL09_TDM.netMx,GEEL09_SD.netMx)

GWSNA09 <- rbind (GWS09_TF.netMx, GWS09_TAM.netMx, GWS09_SDM.netMx, GWS09_TDM.netMx,GWS09_SD.netMx)

HAWNA09 <- rbind (HAW09_G.netMx)

MELBNA09 <- rbind (MELB09_G.netMx,MELB09_SAM.netMx, MELB09_TDM.netMx)

NMFCNA09 <- rbind (NMFC09_G.netMx, NMFC09_B.netMx, NMFC09_SF.netMx, NMFC09_TDM.netMx)

PORTNA09 <- rbind (PORT09_SAM.netMx, PORT09_TDM.netMx)

RICHNA09 <- rbind (RICH09_B.netMx, RICH09_TF.netMx,RICH09_SAM.netMx, RICH09_TAM.netMx, RICH09_SDM.netMx)

STKNA09 <- rbind (STK09_G.netMx, STK09_TAM.netMx, STK09_TDM.netMx, STK09_TD.netMx)

SYDNA09 <- rbind (SYD09_SF.netMx, SYD09_TF.netMx,SYD09_SAM.netMx, SYD09_TAM.netMx, SYD09_SDM.netMx,SYD09_SD.netMx)

#WBNA09 

WCENA09 <- rbind (WCE09_B.netMx,WCE09_SAM.netMx, WCE09_SDM.netMx, WCE09_TDM.netMx)

#Combine all round 9 data
RD9_na <- rbind (ADELNA09, BLNA09, CARLNA09, COLLNA09, ESSNA09, FRENA09, GCFCNA09, GEELNA09, GWSNA09, HAWNA09, MELBNA09, NMFCNA09, PORTNA09, RICHNA09, STKNA09, SYDNA09, WCENA09)

#############################################################################
# ROUND 10
#############################################################################

#ADELNA10 <- rbind (ADEL10_G.netMx, ADEL10_B.netMx, ADEL10_SF.netMx, ADEL10_TF.netMx,ADEL10_SAM.netMx, ADEL10_TAM.netMx, ADEL10_SDM.netMx, ADEL10_TDM.netMx,ADEL10_SD.netMx, ADEL10_TD.netMx, ADEL10_QT.netMx)
WCE10_G.netMx
WCE10_B.netMx
WCE10_SF.netMx
WCE10_TF.netMx
WCE10_SAM.netMx
WCE10_TAM.netMx
WCE10_SDM.netMx
WCE10_TDM.netMx
WCE10_SD.netMx
WCE10_TD.netMx 
WCE10_QT.netMx

ADELNA10 <- rbind (ADEL10_G.netMx, ADEL10_TF.netMx)

BLNA10 <- rbind (BL10_G.netMx, BL10_TF.netMx,BL10_SAM.netMx, BL10_SDM.netMx, BL10_TDM.netMx)

CARLNA10 <- rbind (CARL10_TF.netMx,CARL10_SAM.netMx, CARL10_SDM.netMx, CARL10_TDM.netMx)

COLLNA10 <- rbind (COLL10_SF.netMx, COLL10_TF.netMx, COLL10_TAM.netMx, COLL10_SDM.netMx)

ESSNA10 <- rbind (ESS10_B.netMx,ESS10_TAM.netMx, ESS10_SDM.netMx, ESS10_TDM.netMx)

FRENA10 <- rbind (FRE10_TF.netMx, FRE10_TDM.netMx,FRE10_SD.netMx)

GCFCNA10 <- rbind (GCFC10_TAM.netMx, GCFC10_SDM.netMx, GCFC10_TDM.netMx)

GEELNA10 <- rbind (GEEL10_TF.netMx, GEEL10_TAM.netMx, GEEL10_TDM.netMx)

GWSNA10 <- rbind (GWS10_B.netMx, GWS10_SDM.netMx, GWS10_TD.netMx)

HAWNA10 <- rbind (HAW10_G.netMx, HAW10_B.netMx, HAW10_SF.netMx, HAW10_TF.netMx, HAW10_TAM.netMx)

MELBNA10 <- rbind (MELB10_G.netMx, MELB10_TAM.netMx, MELB10_TDM.netMx)

NMFCNA10 <- rbind (NMFC10_B.netMx, NMFC10_SF.netMx, NMFC10_TAM.netMx, NMFC10_SDM.netMx, NMFC10_TDM.netMx)

PORTNA10 <- rbind (PORT10_SDM.netMx, PORT10_TDM.netMx)

RICHNA10 <- rbind (RICH10_SAM.netMx, RICH10_TAM.netMx, RICH10_SDM.netMx, RICH10_QT.netMx)

STKNA10 <- rbind (STK10_TAM.netMx, STK10_SDM.netMx, STK10_TDM.netMx, STK10_TD.netMx)

SYDNA10 <- rbind (SYD10_G.netMx, SYD10_SF.netMx, SYD10_TAM.netMx, SYD10_SDM.netMx, SYD10_TDM.netMx, SYD10_TD.netMx)

WBNA10 <- rbind (WB10_TDM.netMx)

WCENA10 <- rbind (WCE10_G.netMx, WCE10_TF.netMx,WCE10_SAM.netMx, WCE10_TAM.netMx)

#Combine all round 10 data
RD10_na <- rbind (ADELNA10, BLNA10, CARLNA10, COLLNA10, ESSNA10, FRENA10, GCFCNA10, GEELNA10, GWSNA10, HAWNA10, MELBNA10, NMFCNA10, PORTNA10, RICHNA10, STKNA10, SYDNA10, WCENA10)

#############################################################################
# ROUND 11
#############################################################################

#ADELNA11 <- rbind (ADEL11_G.netMx, ADEL11_B.netMx, ADEL11_SF.netMx, ADEL11_TF.netMx,ADEL11_SAM.netMx, ADEL11_TAM.netMx, ADEL11_SDM.netMx, ADEL11_TDM.netMx,ADEL11_SD.netMx, ADEL11_TD.netMx, ADEL11_QT.netMx)
WCE11_G.netMx
WCE11_B.netMx
WCE11_SF.netMx
WCE11_TF.netMx
WCE11_SAM.netMx
WCE11_TAM.netMx
WCE11_SDM.netMx
WCE11_TDM.netMx
WCE11_SD.netMx
WCE11_TD.netMx 
WCE11_QT.netMx

#ADELNA11

#BLNA11

#CARLNA11

COLLNA11 <- rbind (COLL11_TF.netMx,COLL11_SAM.netMx, COLL11_TAM.netMx, COLL11_SDM.netMx)
  
ESSNA11 <- rbind (ESS11_TF.netMx, ESS11_TAM.netMx, ESS11_TDM.netMx, ESS11_TD.netMx)

FRENA11 <- rbind (FRE11_SAM.netMx, FRE11_TAM.netMx)

GCFCNA11 <- rbind (GCFC11_G.netMx,GCFC11_SAM.netMx, GCFC11_TAM.netMx, GCFC11_TDM.netMx)

GEELNA11 <- rbind (GEEL11_SAM.netMx, GEEL11_TDM.netMx)

GWSNA11 <- rbind (GWS11_B.netMx, GWS11_TAM.netMx, GWS11_TDM.netMx,GWS11_SD.netMx)

#HAWNA11

MELBNA11 <- rbind (MELB11_B.netMx, MELB11_TF.netMx, MELB11_SDM.netMx, MELB11_TDM.netMx)

NMFCNA11 <- rbind (NMFC11_G.netMx, NMFC11_SF.netMx,NMFC11_TAM.netMx, NMFC11_SDM.netMx, NMFC11_TDM.netMx)

PORTNA11 <- rbind (PORT11_SDM.netMx, PORT11_TDM.netMx)

#RICHNA11

STKNA11 <- rbind (STK11_TF.netMx,STK11_SAM.netMx, STK11_SDM.netMx, STK11_TDM.netMx)

SYDNA11 <- rbind (SYD11_G.netMx, SYD11_B.netMx, SYD11_SF.netMx,SYD11_SAM.netMx, SYD11_TAM.netMx, SYD11_SDM.netMx, SYD11_TDM.netMx)

#WBNA11

WCENA11 <- rbind (WCE11_G.netMx, WCE11_TF.netMx,WCE11_SAM.netMx, WCE11_SDM.netMx, WCE11_TDM.netMx)

#Combine all round 11 data
RD11_na <- rbind (COLLNA11, ESSNA11, FRENA11, GCFCNA11, GEELNA11, GWSNA11, MELBNA11, NMFCNA11, PORTNA11, STKNA11, SYDNA11, WCENA11)

#############################################################################
# ROUND 12
#############################################################################

#ADELNA12 <- rbind (ADEL12_G.netMx, ADEL12_B.netMx, ADEL12_SF.netMx, ADEL12_TF.netMx,ADEL12_SAM.netMx, ADEL12_TAM.netMx, ADEL12_SDM.netMx, ADEL12_TDM.netMx,ADEL12_SD.netMx, ADEL12_TD.netMx, ADEL12_QT.netMx)
WCE12_G.netMx
WCE12_B.netMx
WCE12_SF.netMx
WCE12_TF.netMx
WCE12_SAM.netMx
WCE12_TAM.netMx
WCE12_SDM.netMx
WCE12_TDM.netMx
WCE12_SD.netMx
WCE12_TD.netMx 
WCE12_QT.netMx

ADELNA12 <- rbind (ADEL12_B.netMx, ADEL12_TF.netMx, ADEL12_TAM.netMx, ADEL12_TDM.netMx)

BLNA12 <- rbind (BL12_SF.netMx,BL12_SAM.netMx, BL12_TAM.netMx, BL12_SDM.netMx, BL12_TDM.netMx)

CARLNA12 <- rbind (CARL12_SF.netMx,CARL12_SAM.netMx, CARL12_TAM.netMx, CARL12_SDM.netMx, CARL12_TDM.netMx)

#COLLNA12

#ESSNA12

#FRENA12

#GCFCNA12

GEELNA12 <- rbind (GEEL12_G.netMx)

GWSNA12 <- rbind (GWS12_TF.netMx,GWS12_SAM.netMx, GWS12_TAM.netMx, GWS12_TDM.netMx, GWS12_TD.netMx)

HAWNA12 <- rbind (HAW12_TF.netMx,HAW12_SAM.netMx, HAW12_SDM.netMx, HAW12_TDM.netMx)

MELBNA12 <- rbind (MELB12_G.netMx,MELB12_SAM.netMx, MELB12_TAM.netMx, MELB12_SDM.netMx, MELB12_TDM.netMx)

NMFCNA12 <- rbind (NMFC12_G.netMx, NMFC12_TF.netMx,NMFC12_SAM.netMx, NMFC12_SDM.netMx, NMFC12_TDM.netMx)

PORTNA12 <- rbind (PORT12_G.netMx, PORT12_SF.netMx, PORT12_TAM.netMx, PORT12_TDM.netMx)

RICHNA12 <- rbind (RICH12_B.netMx,RICH12_SAM.netMx, RICH12_TAM.netMx, RICH12_TDM.netMx)

#STKNA12

#SYDNA12

WBNA12 <- rbind (WB12_G.netMx,WB12_SAM.netMx, WB12_TAM.netMx, WB12_TDM.netMx)

WCENA12 <- rbind (WCE12_SF.netMx, WCE12_TF.netMx, WCE12_SDM.netMx, WCE12_TDM.netMx)

#Combine all round 12 data
RD12_na <- rbind (ADELNA12, BLNA12, CARLNA12, GEELNA12, GWSNA12, HAWNA12, MELBNA12, NMFCNA12, PORTNA12, RICHNA12,WBNA12, WCENA12)

#############################################################################
# ROUND 13
#############################################################################

#ADELNA13 <- rbind (ADEL13_G.netMx, ADEL13_B.netMx, ADEL13_SF.netMx, ADEL13_TF.netMx,ADEL13_SAM.netMx, ADEL13_TAM.netMx, ADEL13_SDM.netMx, ADEL13_TDM.netMx,ADEL13_SD.netMx, ADEL13_TD.netMx, ADEL13_QT.netMx)
WB13_G.netMx
WB13_B.netMx
WB13_SF.netMx
WB13_TF.netMx
WB13_SAM.netMx
WB13_TAM.netMx
WB13_SDM.netMx
WB13_TDM.netMx
WB13_SD.netMx
WB13_TD.netMx 
WB13_QT.netMx

ADELNA13 <- rbind (ADEL13_TF.netMx,ADEL13_SAM.netMx, ADEL13_TAM.netMx, ADEL13_TDM.netMx)

BLNA13 <- rbind (BL13_TF.netMx,BL13_SAM.netMx, BL13_TAM.netMx, BL13_SDM.netMx, BL13_TDM.netMx)

CARLNA13 <- rbind (CARL13_G.netMx, CARL13_B.netMx, CARL13_SF.netMx,CARL13_SAM.netMx, CARL13_TAM.netMx, CARL13_SDM.netMx, CARL13_TDM.netMx, CARL13_QT.netMx)

COLLNA13 <- rbind (COLL13_SF.netMx, COLL13_TF.netMx, COLL13_TAM.netMx, COLL13_SDM.netMx, COLL13_TDM.netMx,COLL13_SD.netMx)

ESSNA13 <- rbind (ESS13_G.netMx, ESS13_B.netMx, ESS13_TAM.netMx, ESS13_TDM.netMx, ESS13_TD.netMx, ESS13_QT.netMx)

FRENA13 <- rbind (FRE13_G.netMx, FRE13_SDM.netMx)

GCFCNA13 <- rbind (GCFC13_B.netMx, GCFC13_TF.netMx, GCFC13_TAM.netMx, GCFC13_SDM.netMx, GCFC13_TDM.netMx)

#GEELNA13

#GWSNA13

HAWNA13 <- rbind (HAW13_G.netMx, HAW13_TF.netMx,HAW13_SAM.netMx, HAW13_TDM.netMx)

#MELBNA13

#NMFCNA13

#PORTNA13

RICHNA13 <- rbind (RICH13_B.netMx,RICH13_SAM.netMx, RICH13_TAM.netMx, RICH13_SDM.netMx, RICH13_TDM.netMx)

STKNA13 <- rbind (STK13_B.netMx, STK13_TF.netMx,STK13_SAM.netMx, STK13_TAM.netMx, STK13_SDM.netMx, STK13_TDM.netMx)

SYDNA13 <- rbind (SYD13_SF.netMx, SYD13_TF.netMx, SYD13_TAM.netMx, SYD13_TDM.netMx)

WBNA13 <- rbind (WB13_SF.netMx, WB13_TF.netMx, WB13_TAM.netMx, WB13_SDM.netMx, WB13_TDM.netMx)

#WCENA13

#Combine all round 13 data
RD13_na <- rbind (ADELNA13, BLNA13, CARLNA13, COLLNA13, ESSNA13, FRENA13, GCFCNA13, HAWNA13,RICHNA13, STKNA13, SYDNA13, WBNA13)

#############################################################################
# ROUND 14
#############################################################################

#ADELNA14 <- rbind (ADEL14_G.netMx, ADEL14_B.netMx, ADEL14_SF.netMx, ADEL14_TF.netMx,ADEL14_SAM.netMx, ADEL14_TAM.netMx, ADEL14_SDM.netMx, ADEL14_TDM.netMx,ADEL14_SD.netMx, ADEL14_TD.netMx, ADEL14_QT.netMx)
WCE14_G.netMx
WCE14_B.netMx
WCE14_SF.netMx
WCE14_TF.netMx
WCE14_SAM.netMx
WCE14_TAM.netMx
WCE14_SDM.netMx
WCE14_TDM.netMx
WCE14_SD.netMx
WCE14_TD.netMx 
WCE14_QT.netMx

#ADELNA14

BLNA14 <- rbind (BL14_SDM.netMx, BL14_TDM.netMx)

CARLNA14 <- rbind (CARL14_SAM.netMx, CARL14_TAM.netMx, CARL14_SDM.netMx, CARL14_TDM.netMx)

COLLNA14 <- rbind (COLL14_B.netMx,COLL14_SAM.netMx, COLL14_TAM.netMx, COLL14_SDM.netMx)

ESSNA14 <- rbind (ESS14_B.netMx, ESS14_SF.netMx,ESS14_SAM.netMx, ESS14_TAM.netMx, ESS14_SDM.netMx, ESS14_TDM.netMx,ESS14_SD.netMx)

FRENA14 <- rbind (FRE14_G.netMx,FRE14_SAM.netMx, FRE14_TAM.netMx, FRE14_SDM.netMx,FRE14_SD.netMx)

GCFCNA14 <- rbind (GCFC14_B.netMx, GCFC14_SF.netMx,GCFC14_SAM.netMx, GCFC14_SDM.netMx, GCFC14_TDM.netMx, GCFC14_QT.netMx)

#GEELNA14

GWSNA14 <- rbind (GWS14_B.netMx, GWS14_TAM.netMx, GWS14_SDM.netMx, GWS14_TDM.netMx)

HAWNA14 <- rbind (HAW14_B.netMx,HAW14_SAM.netMx, HAW14_TAM.netMx, HAW14_SDM.netMx, HAW14_TDM.netMx, HAW14_TD.netMx)

MELBNA14 <- rbind (MELB14_TF.netMx,MELB14_SAM.netMx, MELB14_TAM.netMx, MELB14_SDM.netMx, MELB14_TDM.netMx, MELB14_QT.netMx)

NMFCNA14 <- rbind (NMFC14_TAM.netMx, NMFC14_SDM.netMx, NMFC14_TDM.netMx, NMFC14_TD.netMx)

PORTNA14 <- rbind (PORT14_G.netMx, PORT14_B.netMx, PORT14_SF.netMx,PORT14_SAM.netMx, PORT14_TAM.netMx, PORT14_SDM.netMx, PORT14_TDM.netMx)

RICHNA14 <- rbind (RICH14_G.netMx, RICH14_B.netMx, RICH14_TF.netMx,RICH14_SD.netMx)

STKNA14 <- rbind (STK14_G.netMx, STK14_SDM.netMx)

SYDNA14 <- rbind (SYD14_G.netMx, SYD14_TF.netMx,SYD14_SAM.netMx, SYD14_TAM.netMx)

WBNA14 <- rbind (WB14_G.netMx, WB14_SF.netMx, WB14_TF.netMx, WB14_TAM.netMx, WB14_SDM.netMx, WB14_TDM.netMx)

WCENA14 <- rbind (WCE14_G.netMx, WCE14_TAM.netMx)

#Combine all round 14 data
RD14_na <- rbind (BLNA14, CARLNA14, COLLNA14, ESSNA14, FRENA14, GCFCNA14, GWSNA14, HAWNA14, MELBNA14, NMFCNA14, PORTNA14, RICHNA14, STKNA14, SYDNA14, WBNA14, WCENA14)

#############################################################################
# ROUND 15
#############################################################################

#ADELNA15 <- rbind (ADEL15_G.netMx, ADEL15_B.netMx, ADEL15_SF.netMx, ADEL15_TF.netMx,ADEL15_SAM.netMx, ADEL15_TAM.netMx, ADEL15_SDM.netMx, ADEL15_TDM.netMx,ADEL15_SD.netMx, ADEL15_TD.netMx, ADEL15_QT.netMx)
WCE15_G.netMx
WCE15_B.netMx
WCE15_SF.netMx
WCE15_TF.netMx
WCE15_SAM.netMx
WCE15_TAM.netMx
WCE15_SDM.netMx
WCE15_TDM.netMx
WCE15_SD.netMx
WCE15_TD.netMx 
WCE15_QT.netMx

ADELNA15 <- rbind (ADEL15_G.netMx,ADEL15_SAM.netMx, ADEL15_SDM.netMx, ADEL15_TDM.netMx, ADEL15_TD.netMx)

BLNA15<- rbind (BL15_G.netMx, BL15_B.netMx, BL15_SDM.netMx, BL15_TDM.netMx,BL15_SD.netMx)

CARLNA15 <- rbind (CARL15_G.netMx, CARL15_SF.netMx, CARL15_TAM.netMx, CARL15_TDM.netMx)

COLLNA15 <- rbind (COLL15_TF.netMx, COLL15_TAM.netMx, COLL15_TDM.netMx)

ESSNA15 <- rbind (ESS15_TAM.netMx, ESS15_SDM.netMx, ESS15_TDM.netMx, ESS15_TD.netMx)

FRENA15 <- rbind (FRE15_G.netMx,FRE15_SAM.netMx, FRE15_TAM.netMx, FRE15_SDM.netMx, FRE15_TDM.netMx, FRE15_TD.netMx)

GCFCNA15 <- rbind (GCFC15_G.netMx, GCFC15_TF.netMx,GCFC15_SAM.netMx, GCFC15_TAM.netMx, GCFC15_TDM.netMx)

GEELNA15 <- rbind (GEEL15_G.netMx, GEEL15_B.netMx, GEEL15_TAM.netMx, GEEL15_TDM.netMx)

GWSNA15 <- rbind (GWS15_TF.netMx, GWS15_TAM.netMx, GWS15_SDM.netMx, GWS15_TDM.netMx)

HAWNA15 <- rbind (HAW15_SF.netMx, HAW15_TF.netMx, HAW15_TAM.netMx, HAW15_SDM.netMx)

MELBNA15 <- rbind (MELB15_B.netMx, MELB15_TF.netMx,MELB15_SAM.netMx, MELB15_TDM.netMx)

NMFCNA15 <- rbind (NMFC15_TF.netMx, NMFC15_TAM.netMx, NMFC15_SDM.netMx, NMFC15_TDM.netMx)

PORTNA15 <- rbind (PORT15_SAM.netMx,PORT15_TDM.netMx)

RICHNA15 <- rbind (RICH15_G.netMx, RICH15_SF.netMx, RICH15_TF.netMx, RICH15_TAM.netMx, RICH15_TDM.netMx)

STKNA15 <- rbind (STK15_B.netMx, STK15_SF.netMx, STK15_TF.netMx, STK15_TAM.netMx, STK15_TDM.netMx)

SYDNA15 <- rbind (SYD15_SAM.netMx, SYD15_TAM.netMx, SYD15_TDM.netMx)

WBNA15 <- rbind (WB15_G.netMx,WB15_TF.netMx,WB15_SAM.netMx)

WCENA15 <- rbind (WCE15_G.netMx, WCE15_B.netMx, WCE15_TAM.netMx, WCE15_TDM.netMx)

#Combine all round 15 data
RD15_na <- rbind (ADELNA15, BLNA15, CARLNA15, COLLNA15, ESSNA15, FRENA15, GCFCNA15, GEELNA15, GWSNA15, HAWNA15, MELBNA15, NMFCNA15, PORTNA15, RICHNA15, STKNA15, SYDNA15, WBNA15, WCENA15)

#############################################################################
# ROUND 16
#############################################################################

#ADELNA16 <- rbind (ADEL16_G.netMx, ADEL16_B.netMx, ADEL16_SF.netMx, ADEL16_TF.netMx,ADEL16_SAM.netMx, ADEL16_TAM.netMx, ADEL16_SDM.netMx, ADEL16_TDM.netMx,ADEL16_SD.netMx, ADEL16_TD.netMx, ADEL16_QT.netMx)
WCE16_G.netMx
WCE16_B.netMx
WCE16_SF.netMx
WCE16_TF.netMx
WCE16_SAM.netMx
WCE16_TAM.netMx
WCE16_SDM.netMx
WCE16_TDM.netMx
WCE16_SD.netMx
WCE16_TD.netMx 
WCE16_QT.netMx

ADELNA16 <- rbind (ADEL16_B.netMx, ADEL16_TAM.netMx, ADEL16_SDM.netMx, ADEL16_TDM.netMx,ADEL16_SD.netMx)

BLNA16 <- rbind (BL16_B.netMx, BL16_TF.netMx, BL16_TAM.netMx, BL16_TDM.netMx,BL16_SD.netMx)

CARLNA16 <- rbind (CARL16_G.netMx, CARL16_B.netMx, CARL16_TF.netMx, CARL16_TAM.netMx, CARL16_SDM.netMx, CARL16_TDM.netMx)

COLLNA16 <- rbind (COLL16_SF.netMx, COLL16_TF.netMx, COLL16_SDM.netMx, COLL16_TDM.netMx)

ESSNA16 <- rbind (ESS16_SF.netMx, ESS16_TF.netMx, ESS16_TAM.netMx, ESS16_TDM.netMx)

FRENA16 <- rbind (FRE16_SF.netMx, FRE16_TAM.netMx, FRE16_SDM.netMx)

GCFCNA16 <- rbind (GCFC16_G.netMx, GCFC16_TF.netMx, GCFC16_SDM.netMx, GCFC16_TDM.netMx, GCFC16_TD.netMx)

GEELNA16 <- rbind (GEEL16_G.netMx, GEEL16_TF.netMx,GEEL16_SAM.netMx, GEEL16_TAM.netMx, GEEL16_SDM.netMx, GEEL16_TDM.netMx)

GWSNA16 <- rbind (GWS16_SF.netMx,GWS16_SAM.netMx, GWS16_TAM.netMx, GWS16_SDM.netMx)

HAWNA16 <- rbind (HAW16_G.netMx, HAW16_TAM.netMx, HAW16_TDM.netMx, HAW16_RO.netMx)

MELBNA16 <- rbind (MELB16_B.netMx, MELB16_TF.netMx,MELB16_SAM.netMx, MELB16_SDM.netMx, MELB16_TDM.netMx)

NMFCNA16 <- rbind (NMFC16_G.netMx, NMFC16_TF.netMx, NMFC16_TAM.netMx, NMFC16_SDM.netMx, NMFC16_TDM.netMx)

PORTNA16 <- rbind (PORT16_B.netMx,PORT16_SAM.netMx, PORT16_TDM.netMx)

RICHNA16 <- rbind (RICH16_B.netMx, RICH16_TF.netMx, RICH16_TAM.netMx, RICH16_SDM.netMx, RICH16_TDM.netMx)

STKNA16 <- rbind (STK16_G.netMx, STK16_TF.netMx, STK16_SDM.netMx, STK16_TD.netMx)

SYDNA16 <- rbind (SYD16_SF.netMx, SYD16_TF.netMx, SYD16_SDM.netMx, SYD16_TDM.netMx)

WBNA16 <- rbind (WB16_G.netMx, WB16_TF.netMx, WB16_SDM.netMx, WB16_TDM.netMx)

WCENA16 <- rbind (WCE16_B.netMx, WCE16_SF.netMx, WCE16_TF.netMx,WCE16_SAM.netMx, WCE16_TAM.netMx, WCE16_SDM.netMx, WCE16_TDM.netMx)

#Combine all round 16 data
RD16_na <- rbind (ADELNA16, BLNA16, CARLNA16, COLLNA16, ESSNA16, FRENA16, GCFCNA16, GEELNA16, GWSNA16, HAWNA16, MELBNA16, NMFCNA16, PORTNA16, RICHNA16, STKNA16, SYDNA16, WBNA16, WCENA16)

#############################################################################
# ROUND 17
#############################################################################

#ADELNA17 <- rbind (ADEL17_G.netMx, ADEL17_B.netMx, ADEL17_SF.netMx, ADEL17_TF.netMx,ADEL17_SAM.netMx, ADEL17_TAM.netMx, ADEL17_SDM.netMx, ADEL17_TDM.netMx,ADEL17_SD.netMx, ADEL17_TD.netMx, ADEL17_QT.netMx)
WCE17_G.netMx
WCE17_B.netMx
WCE17_SF.netMx
WCE17_TF.netMx
WCE17_SAM.netMx
WCE17_TAM.netMx
WCE17_SDM.netMx
WCE17_TDM.netMx
WCE17_SD.netMx
WCE17_TD.netMx 
WCE17_QT.netMx

ADELNA17 <- rbind (ADEL17_G.netMx, ADEL17_SF.netMx, ADEL17_TAM.netMx, ADEL17_TDM.netMx)

BLNA17 <- rbind (BL17_TF.netMx, BL17_TAM.netMx, BL17_SDM.netMx, BL17_TDM.netMx,BL17_SD.netMx, BL17_TD.netMx)

CARLNA17 <- rbind (CARL17_B.netMx,CARL17_SAM.netMx, CARL17_SDM.netMx, CARL17_TDM.netMx)

COLLNA17 <- rbind (COLL17_G.netMx,COLL17_SAM.netMx, COLL17_TAM.netMx, COLL17_SDM.netMx)

ESSNA17 <- rbind (ESS17_G.netMx, ESS17_TF.netMx,ESS17_SAM.netMx, ESS17_TAM.netMx, ESS17_TDM.netMx)

FRENA17 <- rbind (FRE17_G.netMx, FRE17_B.netMx, FRE17_SF.netMx, FRE17_TF.netMx,FRE17_SAM.netMx, FRE17_TAM.netMx, FRE17_SDM.netMx, FRE17_TDM.netMx)

GCFCNA17 <- rbind (GCFC17_TF.netMx, GCFC17_TAM.netMx, GCFC17_TDM.netMx)

GEELNA17 <- rbind (GEEL17_G.netMx, GEEL17_TF.netMx,GEEL17_SAM.netMx, GEEL17_TAM.netMx)

GWSNA17 <- rbind (GWS17_G.netMx, GWS17_TF.netMx,GWS17_SAM.netMx, GWS17_TAM.netMx, GWS17_SDM.netMx, GWS17_TDM.netMx, GWS17_TD.netMx)

HAWNA17 <- rbind (HAW17_B.netMx, HAW17_TF.netMx, HAW17_TAM.netMx, HAW17_TDM.netMx)

MELBNA17 <- rbind (MELB17_TAM.netMx, MELB17_SDM.netMx, MELB17_TDM.netMx)

NMFCNA17 <- rbind (NMFC17_G.netMx, NMFC17_B.netMx,NMFC17_SAM.netMx, NMFC17_TDM.netMx)

PORTNA17 <- rbind (PORT17_G.netMx, PORT17_B.netMx, PORT17_TF.netMx, PORT17_SDM.netMx, PORT17_TDM.netMx, PORT17_TD.netMx)

RICHNA17 <- rbind (RICH17_TAM.netMx, RICH17_SDM.netMx, RICH17_TDM.netMx)

STKNA17 <- rbind (STK17_SAM.netMx, STK17_TAM.netMx, STK17_SDM.netMx, STK17_TDM.netMx)

SYDNA17 <- rbind (SYD17_B.netMx, SYD17_SF.netMx, SYD17_TDM.netMx,SYD17_SD.netMx)

WBNA17 <- rbind (WB17_G.netMx, WB17_B.netMx, WB17_TAM.netMx,WB17_SD.netMx, WB17_TD.netMx)

WCENA17 <- rbind (WCE17_SAM.netMx, WCE17_TAM.netMx, WCE17_TDM.netMx, WCE17_TD.netMx)

#Combine all round 17 data
RD17_na <- rbind (ADELNA17, BLNA17, CARLNA17, COLLNA17, ESSNA17, FRENA17, GCFCNA17, GEELNA17, GWSNA17, HAWNA17, MELBNA17, NMFCNA17, PORTNA17, RICHNA17, STKNA17, SYDNA17, WBNA17, WCENA17)

#############################################################################
# ROUND 18
#############################################################################

#ADELNA18 <- rbind (ADEL18_G.netMx, ADEL18_B.netMx, ADEL18_SF.netMx, ADEL18_TF.netMx,ADEL18_SAM.netMx, ADEL18_TAM.netMx, ADEL18_SDM.netMx, ADEL18_TDM.netMx,ADEL18_SD.netMx, ADEL18_TD.netMx, ADEL18_QT.netMx)
WCE18_G.netMx
WCE18_B.netMx
WCE18_SF.netMx
WCE18_TF.netMx
WCE18_SAM.netMx
WCE18_TAM.netMx
WCE18_SDM.netMx
WCE18_TDM.netMx
WCE18_SD.netMx
WCE18_TD.netMx 
WCE18_QT.netMx

ADELNA18 <- rbind (ADEL18_G.netMx, ADEL18_B.netMx, ADEL18_TF.netMx,ADEL18_SAM.netMx, ADEL18_TAM.netMx, ADEL18_SDM.netMx, ADEL18_TDM.netMx)

BLNA18 <- rbind (BL18_G.netMx, BL18_B.netMx, BL18_TF.netMx, BL18_TAM.netMx, BL18_SDM.netMx, BL18_TDM.netMx)

CARLNA18 <- rbind (CARL18_G.netMx, CARL18_B.netMx, CARL18_TAM.netMx, CARL18_SDM.netMx, CARL18_TDM.netMx)

COLLNA18 <- rbind (COLL18_TF.netMx, COLL18_TAM.netMx, COLL18_TD.netMx)

ESSNA18 <- rbind (ESS18_TF.netMx, ESS18_TAM.netMx, ESS18_SDM.netMx, ESS18_TDM.netMx)

FRENA18 <- rbind (FRE18_B.netMx,FRE18_SAM.netMx, FRE18_TAM.netMx, FRE18_TDM.netMx)

GCFCNA18 <- rbind (GCFC18_B.netMx,GCFC18_SAM.netMx, GCFC18_SDM.netMx, GCFC18_TDM.netMx)

GEELNA18 <- rbind (GEEL18_G.netMx, GEEL18_B.netMx, GEEL18_TF.netMx, GEEL18_TAM.netMx, GEEL18_SDM.netMx, GEEL18_TDM.netMx)

GWSNA18 <- rbind (GWS18_TAM.netMx, GWS18_SDM.netMx, GWS18_TDM.netMx, GWS18_QT.netMx)

HAWNA18 <- rbind (HAW18_TF.netMx, HAW18_TAM.netMx, HAW18_SDM.netMx, HAW18_TDM.netMx)

MELBNA18 <- rbind (MELB18_G.netMx, MELB18_SF.netMx, MELB18_TF.netMx, MELB18_TAM.netMx, MELB18_TDM.netMx)

NMFCNA18 <- rbind (NMFC18_TF.netMx, NMFC18_TAM.netMx)

PORTNA18 <- rbind (PORT18_G.netMx, PORT18_B.netMx, PORT18_TF.netMx, PORT18_TAM.netMx, PORT18_TDM.netMx)

RICHNA18 <- rbind (RICH18_SAM.netMx,RICH18_TD.netMx, RICH18_QT.netMx)

STKNA18 <- rbind (STK18_G.netMx, STK18_SDM.netMx, STK18_TDM.netMx)

SYDNA18 <- rbind (SYD18_G.netMx, SYD18_TF.netMx, SYD18_TAM.netMx, SYD18_SDM.netMx, SYD18_TDM.netMx)

WBNA18 <- rbind (WB18_TF.netMx,WB18_SAM.netMx, WB18_TAM.netMx, WB18_SDM.netMx, WB18_TDM.netMx)

WCENA18 <- rbind (WCE18_SF.netMx, WCE18_TAM.netMx, WCE18_SDM.netMx, WCE18_TDM.netMx, WCE18_TD.netMx)

#Combine all round 18 data
RD18_na <- rbind (ADELNA18, BLNA18, CARLNA18, COLLNA18, ESSNA18, FRENA18, GCFCNA18, GEELNA18, GWSNA18, HAWNA18, MELBNA18, NMFCNA18, PORTNA18, RICHNA18, STKNA18, SYDNA18, WBNA18, WCENA18)

#############################################################################
# ROUND 19
#############################################################################

#ADELNA19 <- rbind (ADEL19_G.netMx, ADEL19_B.netMx, ADEL19_SF.netMx, ADEL19_TF.netMx,ADEL19_SAM.netMx, ADEL19_TAM.netMx, ADEL19_SDM.netMx, ADEL19_TDM.netMx,ADEL19_SD.netMx, ADEL19_TD.netMx, ADEL19_QT.netMx)
WCE19_G.netMx
WCE19_B.netMx
WCE19_SF.netMx
WCE19_TF.netMx
WCE19_SAM.netMx
WCE19_TAM.netMx
WCE19_SDM.netMx
WCE19_TDM.netMx
WCE19_SD.netMx
WCE19_TD.netMx 
WCE19_QT.netMx

ADELNA19 <- rbind (ADEL19_TDM.netMx)

BLNA19 <- rbind (BL19_G.netMx, BL19_TF.netMx, BL19_TAM.netMx, BL19_SDM.netMx, BL19_TDM.netMx)

CARLNA19 <- rbind (CARL19_G.netMx, CARL19_TF.netMx,CARL19_SAM.netMx, CARL19_TAM.netMx, CARL19_SDM.netMx)

COLLNA19 <- rbind (COLL19_SF.netMx,COLL19_SAM.netMx, COLL19_TAM.netMx, COLL19_TDM.netMx)

ESSNA19 <- rbind (ESS19_B.netMx, ESS19_TF.netMx, ESS19_TAM.netMx, ESS19_SDM.netMx, ESS19_TDM.netMx, ESS19_TD.netMx)

FRENA19 <- rbind (FRE19_G.netMx, FRE19_TAM.netMx, FRE19_SDM.netMx, FRE19_TDM.netMx)

GCFCNA19 <- rbind (GCFC19_G.netMx, GCFC19_TF.netMx, GCFC19_TDM.netMx)

GEELNA19 <- rbind (GEEL19_SF.netMx, GEEL19_TF.netMx, GEEL19_TAM.netMx, GEEL19_TDM.netMx)

GWSNA19 <- rbind (GWS19_G.netMx, GWS19_TF.netMx, GWS19_TAM.netMx, GWS19_SDM.netMx, GWS19_TDM.netMx)

HAWNA19 <- rbind (HAW19_TAM.netMx)

MELBNA19 <- rbind (MELB19_SAM.netMx, MELB19_TAM.netMx, MELB19_SDM.netMx, MELB19_TDM.netMx)

NMFCNA19 <- rbind (NMFC19_G.netMx, NMFC19_B.netMx, NMFC19_SF.netMx, NMFC19_TF.netMx,NMFC19_SAM.netMx, NMFC19_TDM.netMx)

PORTNA19 <- rbind (PORT19_TF.netMx, PORT19_TAM.netMx, PORT19_SDM.netMx, PORT19_TDM.netMx, PORT19_TD.netMx, PORT19_QT.netMx)

RICHNA19 <- rbind (RICH19_B.netMx, RICH19_SF.netMx, RICH19_TF.netMx, RICH19_TAM.netMx, RICH19_TDM.netMx)

STKNA19 <- rbind (STK19_TF.netMx, STK19_TAM.netMx, STK19_TDM.netMx)

SYDNA19 <- rbind (SYD19_G.netMx, SYD19_B.netMx, SYD19_TF.netMx, SYD19_TAM.netMx, SYD19_SDM.netMx, SYD19_TDM.netMx)

WBNA19 <- rbind (WB19_SF.netMx, WB19_TAM.netMx, WB19_SDM.netMx, WB19_TDM.netMx, WB19_TD.netMx)

WCENA19 <- rbind (WCE19_B.netMx, WCE19_TF.netMx, WCE19_TAM.netMx, WCE19_TDM.netMx)

#Combine all round 19 data
RD19_na <- rbind (ADELNA19, BLNA19, CARLNA19, COLLNA19, ESSNA19, FRENA19, GCFCNA19, GEELNA19, GWSNA19, HAWNA19, MELBNA19, NMFCNA19, PORTNA19, RICHNA19, STKNA19, SYDNA19, WBNA19, WCENA19)

#############################################################################
# ROUND 20
#############################################################################

#ADELNA20 <- rbind (ADEL20_G.netMx, ADEL20_B.netMx, ADEL20_SF.netMx, ADEL20_TF.netMx,ADEL20_SAM.netMx, ADEL20_TAM.netMx, ADEL20_SDM.netMx, ADEL20_TDM.netMx,ADEL20_SD.netMx, ADEL20_TD.netMx, ADEL20_QT.netMx)
WCE20_G.netMx
WCE20_B.netMx
WCE20_SF.netMx
WCE20_TF.netMx
WCE20_SAM.netMx
WCE20_TAM.netMx
WCE20_SDM.netMx
WCE20_TDM.netMx
WCE20_SD.netMx
WCE20_TD.netMx 
WCE20_QT.netMx

ADELNA20 <- rbind (ADEL20_B.netMx, ADEL20_TF.netMx,ADEL20_SAM.netMx, ADEL20_TAM.netMx, ADEL20_SDM.netMx)

BLNA20 <- rbind (BL20_G.netMx, BL20_SF.netMx, BL20_SDM.netMx, BL20_TDM.netMx)

CARLNA20 <- rbind (CARL20_B.netMx,CARL20_SAM.netMx, CARL20_TAM.netMx, CARL20_SDM.netMx, CARL20_TDM.netMx)

COLLNA20 <- rbind (COLL20_TF.netMx, COLL20_TAM.netMx, COLL20_TDM.netMx)

ESSNA20 <- rbind (ESS20_TF.netMx,ESS20_SAM.netMx, ESS20_TAM.netMx, ESS20_SDM.netMx, ESS20_TDM.netMx)

FRENA20 <- rbind (FRE20_G.netMx, FRE20_B.netMx, FRE20_SF.netMx, FRE20_TF.netMx,FRE20_SAM.netMx, FRE20_SDM.netMx, FRE20_TDM.netMx)

GCFCNA20 <- rbind (GCFC20_TF.netMx,GCFC20_SAM.netMx, GCFC20_TAM.netMx, GCFC20_TDM.netMx)

GEELNA20 <- rbind (GEEL20_G.netMx, GEEL20_TAM.netMx, GEEL20_TDM.netMx)

GWSNA20 <- rbind (GWS20_B.netMx, GWS20_SF.netMx, GWS20_TAM.netMx, GWS20_SDM.netMx, GWS20_TDM.netMx)

HAWNA20 <- rbind (HAW20_G.netMx, HAW20_TF.netMx,HAW20_SAM.netMx, HAW20_TAM.netMx, HAW20_SDM.netMx, HAW20_TDM.netMx)

MELBNA20 <- rbind (MELB20_G.netMx, MELB20_B.netMx, MELB20_TF.netMx, MELB20_SDM.netMx, MELB20_TDM.netMx)

NMFCNA20 <- rbind (NMFC20_B.netMx,NMFC20_SAM.netMx, NMFC20_TAM.netMx, NMFC20_TDM.netMx, NMFC20_TD.netMx)

PORTNA20 <- rbind (PORT20_G.netMx,PORT20_SAM.netMx, PORT20_TAM.netMx, PORT20_SDM.netMx, PORT20_TDM.netMx)

RICHNA20 <- rbind (RICH20_G.netMx, RICH20_SDM.netMx, RICH20_TDM.netMx)

STKNA20 <- rbind (STK20_SF.netMx, STK20_TF.netMx, STK20_TAM.netMx, STK20_SDM.netMx, STK20_TDM.netMx, STK20_TD.netMx)

SYDNA20 <- rbind (SYD20_G.netMx, SYD20_TF.netMx, SYD20_SDM.netMx, SYD20_TDM.netMx, SYD20_TD.netMx)

WBNA20 <- rbind (WB20_G.netMx, WB20_TAM.netMx, WB20_TDM.netMx)

WCENA20 <- rbind (WCE20_G.netMx, WCE20_B.netMx, WCE20_TF.netMx,WCE20_SAM.netMx, WCE20_TAM.netMx, WCE20_TDM.netMx)

#Combine all round 20 data
RD20_na <- rbind (ADELNA20, BLNA20, CARLNA20, COLLNA20, ESSNA20, FRENA20, GCFCNA20, GEELNA20, GWSNA20, HAWNA20, MELBNA20, NMFCNA20, PORTNA20, RICHNA20, STKNA20, SYDNA20, WBNA20, WCENA20)

#############################################################################
# ROUND 21
#############################################################################

#ADELNA21 <- rbind (ADEL21_G.netMx, ADEL21_B.netMx, ADEL21_SF.netMx, ADEL21_TF.netMx,ADEL21_SAM.netMx, ADEL21_TAM.netMx, ADEL21_SDM.netMx, ADEL21_TDM.netMx,ADEL21_SD.netMx, ADEL21_TD.netMx, ADEL21_QT.netMx)
WCE21_G.netMx
WCE21_B.netMx
WCE21_SF.netMx
WCE21_TF.netMx
WCE21_SAM.netMx
WCE21_TAM.netMx
WCE21_SDM.netMx
WCE21_TDM.netMx
WCE21_SD.netMx
WCE21_TD.netMx 
WCE21_QT.netMx

ADELNA21 <- rbind (ADEL21_G.netMx, ADEL21_TF.netMx,ADEL21_SAM.netMx, ADEL21_TDM.netMx)

BLNA21 <- rbind (BL21_G.netMx, BL21_B.netMx, BL21_TF.netMx, BL21_TAM.netMx, BL21_SDM.netMx, BL21_TDM.netMx, BL21_TD.netMx)

CARLNA21 <- rbind (CARL21_TF.netMx,CARL21_SAM.netMx, CARL21_TAM.netMx, CARL21_SDM.netMx, CARL21_TDM.netMx, CARL21_QT.netMx)

COLLNA21 <- rbind (COLL21_G.netMx, COLL21_SF.netMx, COLL21_TF.netMx, COLL21_SDM.netMx, COLL21_TDM.netMx)

ESSNA21 <- rbind (ESS21_G.netMx, ESS21_TF.netMx, ESS21_TAM.netMx, ESS21_SDM.netMx, ESS21_TDM.netMx)

FRENA21 <- rbind (FRE21_G.netMx, FRE21_TF.netMx, FRE21_SDM.netMx)

GCFCNA21 <- rbind (GCFC21_G.netMx,GCFC21_SF.netMx, GCFC21_TF.netMx, GCFC21_TDM.netMx)

GEELNA21 <- rbind (GEEL21_G.netMx, GEEL21_SF.netMx, GEEL21_TF.netMx,GEEL21_SAM.netMx, GEEL21_TAM.netMx, GEEL21_SDM.netMx)

GWSNA21 <- rbind (GWS21_TF.netMx,GWS21_SAM.netMx, GWS21_TAM.netMx, GWS21_SDM.netMx, GWS21_TDM.netMx, GWS21_TD.netMx)

HAWNA21 <- rbind (HAW21_SF.netMx, HAW21_TF.netMx,HAW21_SAM.netMx, HAW21_SDM.netMx, HAW21_TDM.netMx)

MELBNA21 <- rbind (MELB21_G.netMx, MELB21_B.netMx, MELB21_SF.netMx, MELB21_TF.netMx)

NMFCNA21 <- rbind (NMFC21_G.netMx, NMFC21_B.netMx, NMFC21_SDM.netMx, NMFC21_TDM.netMx)

PORTNA21 <- rbind (PORT21_G.netMx, PORT21_SF.netMx, PORT21_TF.netMx,PORT21_TDM.netMx)

RICHNA21 <- rbind (RICH21_G.netMx, RICH21_SF.netMx,RICH21_SAM.netMx, RICH21_TAM.netMx, RICH21_SDM.netMx, RICH21_TDM.netMx)

STKNA21 <- rbind (STK21_TAM.netMx, STK21_TDM.netMx)

SYDNA21 <- rbind (SYD21_SF.netMx,SYD21_SAM.netMx, SYD21_TAM.netMx, SYD21_SDM.netMx, SYD21_QT.netMx)

WBNA21 <- rbind (WB21_G.netMx, WB21_SF.netMx,WB21_SAM.netMx, WB21_TAM.netMx, WB21_TDM.netMx, WB21_TD.netMx)

WCENA21 <- rbind (WCE21_G.netMx, WCE21_B.netMx, WCE21_TDM.netMx)

#Combine all round 21 data
RD21_na <- rbind (ADELNA21, BLNA21, CARLNA21, COLLNA21, ESSNA21, FRENA21, GCFCNA21, GEELNA21, GWSNA21, HAWNA21, MELBNA21, NMFCNA21, PORTNA21, RICHNA21, STKNA21, SYDNA21, WBNA21, WCENA21)

#############################################################################
# ROUND 22
#############################################################################

#ADELNA22 <- rbind (ADEL22_G.netMx, ADEL22_B.netMx, ADEL22_SF.netMx, ADEL22_TF.netMx,ADEL22_SAM.netMx, ADEL22_TAM.netMx, ADEL22_SDM.netMx, ADEL22_TDM.netMx,ADEL22_SD.netMx, ADEL22_TD.netMx, ADEL22_QT.netMx)
WCE22_G.netMx
WCE22_B.netMx
WCE22_SF.netMx
WCE22_TF.netMx
WCE22_SAM.netMx
WCE22_TAM.netMx
WCE22_SDM.netMx
WCE22_TDM.netMx
WCE22_SD.netMx
WCE22_TD.netMx 
WCE22_QT.netMx

ADELNA22 <- rbind (ADEL22_G.netMx, ADEL22_B.netMx, ADEL22_TF.netMx, ADEL22_TDM.netMx)

BLNA22 <- rbind (BL22_G.netMx, BL22_TF.netMx,BL22_SAM.netMx, BL22_TAM.netMx,BL22_SD.netMx)

CARLNA22 <- rbind (CARL22_B.netMx, CARL22_TF.netMx,CARL22_SAM.netMx, CARL22_TAM.netMx, CARL22_SDM.netMx)

COLLNA22<- rbind (COLL22_TF.netMx, COLL22_TAM.netMx, COLL22_TDM.netMx)

ESSNA22 <- rbind (ESS22_B.netMx,ESS22_SAM.netMx, ESS22_TAM.netMx, ESS22_SDM.netMx, ESS22_TDM.netMx, ESS22_TD.netMx)

FRENA22 <- rbind (FRE22_G.netMx, FRE22_SF.netMx, FRE22_TF.netMx, FRE22_SDM.netMx)

GCFCNA22 <- rbind (GCFC22_SF.netMx, GCFC22_TF.netMx, GCFC22_TAM.netMx, GCFC22_SDM.netMx, GCFC22_TDM.netMx)

GEELNA22 <- rbind (GEEL22_SF.netMx, GEEL22_TAM.netMx, GEEL22_SDM.netMx, GEEL22_TDM.netMx, GEEL22_TD.netMx)

GWSNA22 <- rbind (GWS22_SDM.netMx, GWS22_TDM.netMx, GWS22_TD.netMx)

HAWNA22 <- rbind (HAW22_G.netMx, HAW22_TAM.netMx, HAW22_TDM.netMx)

MELBNA22 <- rbind (MELB22_SF.netMx, MELB22_TAM.netMx, MELB22_TDM.netMx)

NMFCNA22 <- rbind (NMFC22_SF.netMx, NMFC22_TF.netMx,NMFC22_SAM.netMx, NMFC22_SDM.netMx, NMFC22_TDM.netMx, NMFC22_TD.netMx)

PORTNA22 <- rbind (PORT22_SAM.netMx, PORT22_TAM.netMx, PORT22_TDM.netMx)

RICHNA22 <- rbind (RICH22_TF.netMx,RICH22_SAM.netMx, RICH22_TAM.netMx)

STKNA22 <- rbind (STK22_TAM.netMx, STK22_SDM.netMx, STK22_TDM.netMx,STK22_SD.netMx, STK22_TD.netMx, STK22_QT.netMx)

SYDNA22 <- rbind (SYD22_G.netMx, SYD22_B.netMx, SYD22_TF.netMx, SYD22_TAM.netMx, SYD22_SDM.netMx, SYD22_TDM.netMx)

WBNA22 <- rbind (WB22_SF.netMx, WB22_TF.netMx,WB22_SAM.netMx, WB22_TAM.netMx, WB22_SDM.netMx, WB22_TDM.netMx, WB22_TD.netMx)

WCENA22 <- rbind (WCE22_G.netMx, WCE22_B.netMx, WCE22_TF.netMx,WCE22_SAM.netMx, WCE22_TAM.netMx)

#Combine all round 22 data
RD22_na <- rbind (ADELNA22, BLNA22, CARLNA22, COLLNA22, ESSNA22, FRENA22, GCFCNA22, GEELNA22, GWSNA22, HAWNA22, MELBNA22, NMFCNA22, PORTNA22, RICHNA22, STKNA22, SYDNA22, WBNA22, WCENA22)

#############################################################################
# ROUND 23
#############################################################################

#ADELNA23 <- rbind (ADEL23_G.netMx, ADEL23_B.netMx, ADEL23_SF.netMx, ADEL23_TF.netMx,ADEL23_SAM.netMx, ADEL23_TAM.netMx, ADEL23_SDM.netMx, ADEL23_TDM.netMx,ADEL23_SD.netMx, ADEL23_TD.netMx, ADEL23_QT.netMx)
WCE23_G.netMx
WCE23_B.netMx
WCE23_SF.netMx
WCE23_TF.netMx
WCE23_SAM.netMx
WCE23_TAM.netMx
WCE23_SDM.netMx
WCE23_TDM.netMx
WCE23_SD.netMx
WCE23_TD.netMx 
WCE23_QT.netMx

ADELNA23 <- rbind (ADEL23_TF.netMx,ADEL23_SAM.netMx, ADEL23_TAM.netMx, ADEL23_TDM.netMx, ADEL23_TD.netMx)

BLNA23 <- rbind (BL23_G.netMx, BL23_TDM.netMx, BL23_TD.netMx)

CARLNA23 <- rbind (CARL23_G.netMx, CARL23_TF.netMx,CARL23_SAM.netMx, CARL23_TAM.netMx, CARL23_TD.netMx)

COLLNA23 <- rbind (COLL23_B.netMx, COLL23_TF.netMx,COLL23_SAM.netMx, COLL23_SDM.netMx, COLL23_TDM.netMx)

ESSNA23 <- rbind (ESS23_G.netMx, ESS23_B.netMx, ESS23_SF.netMx,ESS23_TDM.netMx)

FRENA23 <- rbind (FRE23_G.netMx,FRE23_TAM.netMx, FRE23_TDM.netMx)

GCFCNA23 <- rbind (GCFC23_TF.netMx, GCFC23_TAM.netMx, GCFC23_TDM.netMx)

GEELNA23 <- rbind (GEEL23_G.netMx, GEEL23_SF.netMx,GEEL23_SAM.netMx, GEEL23_TAM.netMx, GEEL23_TDM.netMx, GEEL23_TD.netMx)

GWSNA23 <- rbind (GWS23_B.netMx, GWS23_SF.netMx, GWS23_TF.netMx, GWS23_TAM.netMx, GWS23_TDM.netMx)

HAWNA23 <- rbind (HAW23_SF.netMx, HAW23_TF.netMx, HAW23_SDM.netMx, HAW23_TDM.netMx, HAW23_QT.netMx)

MELBNA23 <- rbind (MELB23_G.netMx, MELB23_TAM.netMx, MELB23_SDM.netMx, MELB23_TDM.netMx)

NMFCNA23 <- rbind (NMFC23_G.netMx, NMFC23_TF.netMx, NMFC23_TAM.netMx, NMFC23_SDM.netMx, NMFC23_TDM.netMx)

PORTNA23 <- rbind (PORT23_B.netMx,PORT23_TAM.netMx,PORT23_QT.netMx)

RICHNA23 <- rbind (RICH23_G.netMx, RICH23_B.netMx, RICH23_TAM.netMx, RICH23_SDM.netMx, RICH23_TDM.netMx)

STKNA23 <- rbind (STK23_G.netMx, STK23_B.netMx, STK23_TF.netMx,STK23_SAM.netMx, STK23_TAM.netMx, STK23_SDM.netMx, STK23_TDM.netMx)

SYDNA23 <- rbind (SYD23_G.netMx, SYD23_B.netMx, SYD23_TDM.netMx, SYD23_TD.netMx)

WBNA23 <- rbind (WB23_TF.netMx, WB23_TAM.netMx, WB23_SDM.netMx, WB23_TDM.netMx)

WCENA23 <- rbind (WCE23_TF.netMx, WCE23_TAM.netMx, WCE23_SDM.netMx)

#Combine all round 23 data
RD23_na <- rbind (ADELNA23, BLNA23, CARLNA23, COLLNA23, ESSNA23, FRENA23, GCFCNA23, GEELNA23, GWSNA23, HAWNA23, MELBNA23, NMFCNA23, PORTNA23, RICHNA23, STKNA23, SYDNA23, WBNA23, WCENA23)

#############################################################################
# COMBINE ALL DATA 
#############################################################################
NADF <-rbind (RD1_na,RD2_na, RD3_na, RD4_na, RD5_na, RD6_na, RD7_na, RD8_na, RD9_na, RD10_na, RD11_na, RD12_na, RD13_na, RD14_na, RD15_na, RD16_na, RD17_na, RD18_na, RD19_na, RD20_na, RD21_na, RD22_na, RD23_na)
#View(NADF)

NADF <-as.data.frame(NADF)

# Specify data class for all variables 
NADF$round <- as.factor(NADF$round)
NADF$team <- as.factor(NADF$team)
NADF$kickInOutcome <- as.factor(as.character(NADF$kickInOutcome))
NADF$clusterCoef <- as.numeric(as.character(NADF$clusterCoef))
NADF$degreeCent <- as.numeric(as.character(NADF$degreeCent))
NADF$netDensity <- as.numeric(as.character(NADF$netDensity))
NADF$entropy <- as.numeric(as.character(NADF$entropy))

#Change to 2 decimal places
is.num <- sapply(NADF, is.numeric)
NADF[is.num] <- lapply(NADF[is.num], round, 3)

#Delete end of quarter 
NADF <- NADF[ which(NADF$kickInOutcome != "End of Qtr_DM"), ]

#Add column for team ladder bracket 
# Subset list of team names (unique)
team <- as.data.frame(unique(unlist(NADF$team)))
colnames(team) <- "team"
team$bracket <- team$team
team$bracket <- revalue(team$bracket, c(
  "ADEL" = "Middle 6",
"BL" = "Bottom 6",
"CARL" = "Bottom 6",
"COLL" = "Middle 6",
"ESS" = "Bottom 6",
"FRE" = "Top 6",
"GCFC" = "Bottom 6",
"GEEL" = "Middle 6",
"GWS" = "Middle 6",
"HAW" = "Top 6",
"MELB" = "Bottom 6",
"NMFC" = "Middle 6",
"PORT" = "Middle 6",
"RICH" = "Top 6",
"STK" = "Bottom 6",
"SYD" = "Top 6",
"WB" = "Top 6",  
"WCE" = "Top 6"))

bracket <- team

# Merge with NADF
NADF <- merge(NADF, bracket, by="team")



#Add column for win/loss/draw
# Match outcome
# Subset list of round numbers and team names
matchOutcome <- NADF[,c(1:2)]
matchOutcome <- transform(matchOutcome, newcol=paste(round, team, sep="_"))
matchOutcome$dup <- duplicated(matchOutcome$newcol)
matchOutcome <- matchOutcome[ which(matchOutcome$dup == FALSE), ]
matchOutcome <- matchOutcome[,c(1:3)]
matchOutcome$matchOutcome <- matchOutcome$newcol
matchOutcome$newcol <- revalue(matchOutcome$newcol, c(
  "1_ADEL" = "Win",
  "2_ADEL" = "Win",
  "3_ADEL" = "Win",
  "4_ADEL" = "Loss",
  "5_ADEL" = "Loss",
  "6_ADEL" = "Win",
  "7_ADEL" = "Win",
  "8_ADEL" = "Loss",
  "9_ADEL" = "Loss",
  "10_ADEL" = "Win",
  "12_ADEL" = "Loss",
  "13_ADEL" = "Win",
  "14_ADEL" = "Draw",
  "15_ADEL" = "Loss",
  "16_ADEL" = "Win",
  "17_ADEL" = "Win",
  "18_ADEL" = "Loss",
  "19_ADEL" = "Win",
  "20_ADEL" = "Win",
  "21_ADEL" = "Win",
  "22_ADEL" = "Win",
  "23_ADEL" = "Loss",
  "1_BL" = "Loss",
  "2_BL" = "Loss",
  "3_BL" = "Loss",
  "4_BL" = "Loss",
  "5_BL" = "Loss",
  "6_BL" = "Win",
  "7_BL" = "Win",
  "8_BL" = "Loss",
  "9_BL" = "Loss",
  "10_BL" = "Loss",
  "12_BL" = "Loss",
  "13_BL" = "Loss",
  "14_BL" = "Loss",
  "15_BL"= "Loss",
  "16_BL" = "Loss",
  "17_BL" = "Loss",
  "18_BL" = "Loss",
  "19_BL" = "Loss",
  "20_BL" = "Win",
  "21_BL" = "Loss",
  "22_BL" = "Loss",
  "23_BL" = "Win",
  "1_CARL" = "Loss",
  "2_CARL" = "Loss",
  "3_CARL" = "Loss",
  "4_CARL" = "Win",
  "5_CARL" = "Loss",
  "6_CARL" = "Loss",
  "7_CARL" = "Loss",
  "8_CARL" = "Loss",
  "9_CARL" = "Loss",
  "10_CARL" = "Loss",
  "12_CARL" = "Win",
  "13_CARL" = "Win",
  "14_CARL" = "Loss",
  "15_CARL" = "Loss",
  "16_CARL" = "Loss",
  "17_CARL" = "Loss",
  "18_CARL" = "Loss",
  "19_CARL" = "Loss",
  "20_CARL" = "Loss",
  "21_CARL" = "Win",
  "22_CARL" = "Loss",
  "23_CARL" = "Loss",
  "1_COLL" = "Win",
  "2_COLL" = "Loss",
  "3_COLL" = "Win",
  "4_COLL" = "Win",
  "5_COLL" = "Win",
  "6_COLL" = "Loss",
  "7_COLL" = "Loss",
  "8_COLL" = "Win",
  "9_COLL" = "Win",
  "10_COLL" = "Win",
  "11_COLL" = "Win",
  "13_COLL" = "Loss",
  "14_COLL" = "Loss",
  "15_COLL" = "Loss",
  "16_COLL" = "Loss",
  "17_COLL" = "Loss",
  "18_COLL" = "Loss",
  "19_COLL" = "Win",
  "20_COLL" = "Loss",
  "21_COLL" = "Loss",
  "22_COLL" = "Win",
  "23_COLL" = "Loss",
  "1_ESS" = "Loss",
  "2_ESS" = "Win",
  "3_ESS" = "Win",
  "4_ESS" = "Loss",
  "5_ESS" = "Win",
  "6_ESS" = "Loss",
  "7_ESS" = "Loss",
  "8_ESS" = "Win",
  "9_ESS" = "Loss",
  "10_ESS" = "Loss",
  "11_ESS" = "Loss",
  "13_ESS" = "Loss",
  "14_ESS" = "Loss",
  "15_ESS" = "Win",
  "16_ESS" = "Loss",
  "17_ESS" = "Loss",
  "18_ESS" = "Loss",
  "19_ESS" = "Loss",
  "20_ESS" = "Loss",
  "21_ESS" = "Loss",
  "22_ESS" = "Loss",
  "23_ESS" = "Win",
  "1_FRE" = "Win",
  "2_FRE" = "Win",
  "3_FRE" = "Win",
  "4_FRE" = "Win",
  "5_FRE" = "Win",
  "6_FRE" = "Win",
  "7_FRE" = "Win",
  "8_FRE" = "Win",
  "9_FRE" = "Win",
  "10_FRE" = "Loss",
  "11_FRE" = "Win",
  "13_FRE" = "Win",
  "14_FRE" = "Win",
  "15_FRE" = "Loss",
  "16_FRE" = "Win",
  "17_FRE" = "Win",
  "18_FRE" ="Win",
  "19_FRE" = "Win",
  "20_FRE" = "Loss",
  "21_FRE" = "Loss",
  "22_FRE" = "Win",
  "23_FRE" = "Loss",
  "1_GWS" = "Win",
  "2_GWS" = "Win",
  "3_GWS" = "Loss",
  "4_GWS" = "Win",
  "5_GWS"= "Loss",
  "6_GWS" = "Win",
  "7_GWS" = "Win",
  "8_GWS" = "Win",
  "9_GWS" = "Loss",
  "10_GWS" = "Win",
  "11_GWS" = "Loss",
  "12_GWS" = "Loss",
  "14_GWS" = "Loss",
  "15_GWS" = "Win",
  "16_GWS" = "Win",
  "17_GWS" = "Loss",
  "18_GWS" = "Loss",
  "19_GWS" = "Win",
  "20_GWS" = "Loss",
  "21_GWS" = "Loss",
  "22_GWS" = "Win",
  "23_GWS" = "Loss",
  "1_GEEL" = "Loss",
  "2_GEEL" = "Loss",
  "3_GEEL" = "Win",
  "4_GEEL" = "Loss",
  "5_GEEL" = "Win",
  "6_GEEL" = "Win",
  "7_GEEL" = "Loss",
  "8_GEEL" = "Win",
  "9_GEEL" = "Loss",
  "10_GEEL" = "Win",
  "11_GEEL" = "Win",
  "12_GEEL" = "Loss",
  "14_GEEL" = "Draw",
  "15_GEEL" = "Loss",
  "16_GEEL" = "Win",
  "17_GEEL" = "Win",
  "18_GEEL" = "Win",
  "19_GEEL" = "Win",
  "20_GEEL" = "Loss",
  "21_GEEL" = "Draw",
  "22_GEEL" = "Loss",
  "23_GEEL" = "Win",
  "1_GCFC" = "Loss",
  "2_GCFC" = "Loss",
  "3_GCFC" = "Loss",
  "4_GCFC" = "Loss",
  "5_GCFC" = "Win",
  "6_GCFC" = "Loss",
  "7_GCFC" = "Loss",
  "8_GCFC" = "Loss",
  "9_GCFC" = "Loss",
  "10_GCFC" = "Loss",
  "11_GCFC" = "Loss",
  "13_GCFC" = "Loss",
  "14_GCFC" = "Win",
  "15_GCFC" = "Loss",
  "16_GCFC" = "Loss",
  "17_GCFC" = "Loss",
  "18_GCFC" = "Draw",
  "19_GCFC" = "Win",
  "20_GCFC" = "Loss",
  "21_GCFC" = "Win",
  "22_GCFC" = "Loss",
  "23_GCFC" = "Loss",
  "1_HAW" = "Win",
  "2_HAW" = "Loss",
  "3_HAW" = "Win",
  "4_HAW" = "Loss",
  "5_HAW" = "Win",
  "6_HAW" = "Loss",
  "7_HAW" = "Win",
  "8_HAW" = "Loss",
  "9_HAW" = "Win",
  "10_HAW" = "Win",
  "12_HAW" = "Win",
  "13_HAW" = "Win",
  "14_HAW" = "Win",
  "15_HAW" = "Win",
  "16_HAW" = "Win",
  "17_HAW" = "Win",
  "18_HAW" = "Loss",
  "19_HAW" = "Win",
  "20_HAW" = "Win",
  "21_HAW" = "Loss",
  "22_HAW" = "Win",
  "23_HAW" = "Win",
  "1_MELB" = "Win",
  "2_MELB" = "Loss",
  "3_MELB" = "Loss",
  "4_MELB" = "Win",
  "5_MELB" = "Loss",
  "6_MELB" = "Loss",
  "7_MELB" = "Loss",
  "8_MELB" = "Win",
  "9_MELB" = "Loss",
  "10_MELB" = "Loss",
  "11_MELB" = "Loss",
  "12_MELB" = "Win",
  "14_MELB" = "Loss",
  "15_MELB" = "Loss",
  "16_MELB" = "Win",
  "17_MELB" = "Loss",
  "18_MELB" = "Win",
  "19_MELB" = "Loss",
  "20_MELB" = "Loss",
  "21_MELB" = "Loss",
  "22_MELB" = "Loss",
  "23_MELB" = "Win",
  "1_NMFC" = "Loss",
  "2_NMFC" = "Win",
  "3_NMFC" = "Loss",
  "4_NMFC" = "Win",
  "5_NMFC" = "Loss",
  "6_NMFC" = "Win",
  "7_NMFC" = "Win",
  "8_NMFC" = "Loss",
  "9_NMFC" = "Loss",
  "10_NMFC" = "Win",
  "11_NMFC" = "Loss",
  "12_NMFC" = "Win",
  "14_NMFC" = "Loss",
  "15_NMFC" = "Win",
  "16_NMFC" = "Win",
  "17_NMFC" = "Win",
  "18_NMFC" = "Win",
  "19_NMFC" = "Win",
  "20_NMFC" = "Win",
  "21_NMFC" = "Win",
  "22_NMFC" = "Loss",
  "23_NMFC"= "Loss", 
  "1_PORT" = "Loss",
  "2_PORT" = "Loss",
  "3_PORT" = "Win",
  "4_PORT" = "Win",
  "5_PORT" = "Win",
  "6_PORT" = "Loss",
  "7_PORT" = "Loss",
  "8_PORT" = "Loss",
  "9_PORT" = "Win",
  "10_PORT" = "Win",
  "11_PORT" = "Loss",
  "12_PORT" = "Loss",
  "14_PORT" = "Loss",
  "15_PORT" = "Win", 
  "16_PORT" = "Loss",
  "17_PORT" = "Win",
  "18_PORT" = "Win",
  "19_PORT" = "Loss",
  "20_PORT" = "Win",
  "21_PORT" = "Win",
  "22_PORT" = "Win",
  "23_PORT" = "Win",
  "1_RICH" = "Win",
  "2_RICH" = "Loss",
  "3_RICH" = "Win",
  "4_RICH" = "Loss", 
  "5_RICH" = "Loss",
  "6_RICH" = "Loss",
  "7_RICH" = "Win",
  "8_RICH" = "Win",
  "9_RICH" = "Win",
  "10_RICH" = "Win",
  "12_RICH" = "Loss",
  "13_RICH" = "Win",
  "14_RICH" = "Win",
  "15_RICH" = "Win",
  "16_RICH" = "Win",
  "17_RICH" = "Loss",
  "18_RICH" = "Win",
  "19_RICH" = "Loss",
  "20_RICH" = "Win",
  "21_RICH" = "Win",
  "22_RICH" = "Win",
  "23_RICH" = "Win",
  "1_STK" = "Loss",
  "2_STK" = "Win",
  "3_STK" = "Loss",
  "4_STK" = "Loss",
  "5_STK" = "Loss",
  "6_STK" = "Win",
  "7_STK" = "Loss",
  "8_STK" = "Loss",
  "9_STK" = "Win",
  "10_STK" = "Loss",
  "11_STK" = "Win",
  "13_STK" = "Loss",
  "14_STK" = "Win",
  "15_STK" = "Loss",
  "16_STK" = "Loss",
  "17_STK" = "Win",
  "18_STK" = "Loss",
  "19_STK" = "Loss",
  "20_STK" = "Loss",
  "21_STK" = "Draw",
  "22_STK" = "Loss",
  "23_STK" = "Loss",
  "1_SYD" = "Win",
  "2_SYD" = "Win",
  "3_SYD" = "Win",
  "4_SYD" = "Loss",
  "5_SYD" = "Loss",
  "6_SYD" = "Win",
  "7_SYD" = "Win",
  "8_SYD" = "Win",
  "9_SYD" = "Win",
  "10_SYD" = "Win",
  "11_SYD" = "Win",
  "13_SYD" = "Loss",
  "14_SYD" = "Win",
  "15_SYD" = "Win",
  "16_SYD" = "Loss",
  "17_SYD" = "Loss",
  "18_SYD" = "Win",
  "19_SYD" = "Loss",
  "20_SYD" = "Win",
  "21_SYD" = "Win",
  "22_SYD" = "Win",
  "23_SYD" = "Win",
  "1_WCE" = "Loss",
  "2_WCE" = "Win",
  "3_WCE" = "Loss",
  "4_WCE" = "Win",
  "5_WCE" = "Win",
  "6_WCE" = "Win",
  "7_WCE" = "Win",
  "8_WCE" = "Win",
  "9_WCE" = "Win",
  "10_WCE" = "Loss",
  "11_WCE" = "Win",
  "12_WCE" = "Win",
  "14_WCE" = "Win",
  "15_WCE" = "Win",
  "16_WCE" = "Win",
  "17_WCE" = "Win",
  "18_WCE" = "Draw",
  "19_WCE" = "Loss",
  "20_WCE" = "Win",
  "21_WCE" = "Win",
  "22_WCE" = "Loss",
  "23_WCE" = "Win",
  "1_WB" = "Win",
  "2_WB" = "Win",
  "3_WB" = "Loss",
  "4_WB" = "Win",
  "5_WB" = "Win",
  "6_WB" = "Loss",
  "7_WB" = "Loss",
  "8_WB" = "Loss",
  "9_WB" = "Win",
  "10_WB" = "Loss",
  "12_WB" = "Win",
  "13_WB" = "Win",
  "14_WB" = "Win",
  "15_WB" = "Win",
  "16_WB" = "Loss",
  "17_WB" = "Win",
  "18_WB" = "Win",
  "19_WB" = "Win",
  "20_WB" = "Win",
  "21_WB" = "Loss",
  "22_WB" = "Win",
  "23_WB" = "Loss"))

newcol <- matchOutcome

# Merge with NADF
NADF <- merge(NADF, newcol, by=c("round", "team"))

#Delete unwanted columns
NADF <- NADF[, c(1:9)]

varnames <- c("round", "team", "kickInOutcome", "clusterCoef","degreeCent", "netDensity", "entropy", "bracket", "matchOutcome")
colnames(NADF) <- varnames

#Reorder
NADF <- NADF[,c(1:2, 8:9, 3:7)]


#############################################################################

# Save NADF data frame to CSV

#############################################################################

write.csv(file = "NADF.csv", NADF, row.names = F)