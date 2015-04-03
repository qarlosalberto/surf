-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : TenGigEthGth7.vhd
-- Author     : Larry Ruckman <ruckman@slac.stanford.edu>
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-02-12
-- Last update: 2015-03-30
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 10GBASE-R Ethernet for Gth7
-------------------------------------------------------------------------------
-- Copyright (c) 2015 SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.StdRtlPkg.all;
use work.AxiStreamPkg.all;
use work.AxiLitePkg.all;
use work.TenGigEthPkg.all;
use work.TenGigEthGth7Pkg.all;

entity TenGigEthGth7 is
   -- Defaults:
   -- 9 bits = 4kbytes
   -- 255 x 8 = 2kbytes (not enough for pause)
   -- 11 bits = 16kbytes 
   generic (
      TPD_G              : time                  := 1 ns;
      -- DMA/MAC Configurations
      IB_ADDR_WIDTH_G    : integer               := 11;
      OB_ADDR_WIDTH_G    : integer               := 9;
      PAUSE_THOLD_G      : integer               := 512;
      VALID_THOLD_G      : integer               := 255;
      EOH_BIT_G          : integer               := 0;
      ERR_BIT_G          : integer               := 1;
      HEADER_SIZE_G      : integer               := 16;
      SHIFT_EN_G         : boolean               := false;
      MAC_ADDR_G         : slv(47 downto 0)      := TEN_GIG_ETH_MAC_ADDR_INIT_C;
      -- AXI-Lite Configurations
      AXI_ERROR_RESP_G   : slv(1 downto 0)       := AXI_RESP_SLVERR_C;
      STATUS_CNT_WIDTH_G : natural range 1 to 32 := 32;
      -- AXI Streaming Configurations
      AXIS_CONFIG_G      : AxiStreamConfigType   := AXI_STREAM_CONFIG_INIT_C);  -- Note: Only support 64-bit AXIS configurations
   port (
      -- Streaming DMA Interface 
      dmaClk             : in  sl;
      dmaRst             : in  sl;
      dmaIbMaster        : out AxiStreamMasterType;
      dmaIbSlave         : in  AxiStreamSlaveType;
      dmaObMaster        : in  AxiStreamMasterType;
      dmaObSlave         : out AxiStreamSlaveType;
      -- Slave AXI-Lite Interface 
      axiLiteClk         : in  sl                     := '0';
      axiLiteRst         : in  sl                     := '0';
      axiLiteReadMaster  : in  AxiLiteReadMasterType  := AXI_LITE_READ_MASTER_INIT_C;
      axiLiteReadSlave   : out AxiLiteReadSlaveType;
      axiLiteWriteMaster : in  AxiLiteWriteMasterType := AXI_LITE_WRITE_MASTER_INIT_C;
      axiLiteWriteSlave  : out AxiLiteWriteSlaveType;
      -- SFP+ Ports
      sigDet             : in  sl                     := '1';
      txFault            : in  sl                     := '0';
      txDisable          : out sl;
      -- Misc. Signals
      extRst             : in  sl;
      phyClk             : in  sl;
      phyRst             : in  sl;
      phyReady           : out sl;
      -- Quad PLL Ports
      qplllock           : in  sl;
      qplloutclk         : in  sl;
      qplloutrefclk      : in  sl;
      qpllRst            : out sl;
      -- MGT Ports
      gtTxP              : out sl;
      gtTxN              : out sl;
      gtRxP              : in  sl;
      gtRxN              : in  sl);  
end TenGigEthGth7;

architecture mapping of TenGigEthGth7 is

   signal mAxiReadMaster  : AxiLiteReadMasterType;
   signal mAxiReadSlave   : AxiLiteReadSlaveType;
   signal mAxiWriteMaster : AxiLiteWriteMasterType;
   signal mAxiWriteSlave  : AxiLiteWriteSlaveType;

   signal phyRxd : slv(63 downto 0);
   signal phyRxc : slv(7 downto 0);
   signal phyTxd : slv(63 downto 0);
   signal phyTxc : slv(7 downto 0);

   signal areset    : sl;
   signal txClk322  : sl;
   signal txUsrClk  : sl;
   signal txUsrClk2 : sl;
   signal txUsrRdy  : sl;

   signal drpReqGnt : sl;
   signal drpEn     : sl;
   signal drpWe     : sl;
   signal drpAddr   : slv(15 downto 0);
   signal drpDi     : slv(15 downto 0);
   signal drpRdy    : sl;
   signal drpDo     : slv(15 downto 0);

   signal configurationVector : slv(535 downto 0) := (others => '0');
   signal statusVector        : slv(447 downto 0);

   signal config : TenGigEthGth7Config;
   signal status : TenGigEthGth7Status;
   
begin

   phyReady        <= status.phyReady;
   areset          <= extRst or config.softRst;
   status.qplllock <= qplllock;

   ------------------
   -- Synchronization 
   ------------------
   U_AxiLiteAsync : entity work.AxiLiteAsync
      generic map (
         TPD_G => TPD_G)
      port map (
         -- Slave Port
         sAxiClk         => axiLiteClk,
         sAxiClkRst      => axiLiteRst,
         sAxiReadMaster  => axiLiteReadMaster,
         sAxiReadSlave   => axiLiteReadSlave,
         sAxiWriteMaster => axiLiteWriteMaster,
         sAxiWriteSlave  => axiLiteWriteSlave,
         -- Master Port
         mAxiClk         => phyClk,
         mAxiClkRst      => phyRst,
         mAxiReadMaster  => mAxiReadMaster,
         mAxiReadSlave   => mAxiReadSlave,
         mAxiWriteMaster => mAxiWriteMaster,
         mAxiWriteSlave  => mAxiWriteSlave);    

   txDisable <= status.txDisable;

   U_Sync : entity work.SynchronizerVector
      generic map (
         TPD_G   => TPD_G,
         WIDTH_G => 3)
      port map (
         clk        => phyClk,
         -- Input
         dataIn(0)  => sigDet,
         dataIn(1)  => txFault,
         dataIn(2)  => txUsrRdy,
         -- Output
         dataOut(0) => status.sigDet,
         dataOut(1) => status.txFault,
         dataOut(2) => status.txUsrRdy);  

   ---------------------------
   -- 10 Gig Ethernet MAC core
   ---------------------------
   U_TenGigEthMacCore : entity work.TenGigEthMacCore
      generic map (
         TPD_G           => TPD_G,
         IB_ADDR_WIDTH_G => IB_ADDR_WIDTH_G,
         OB_ADDR_WIDTH_G => OB_ADDR_WIDTH_G,
         PAUSE_THOLD_G   => PAUSE_THOLD_G,
         VALID_THOLD_G   => VALID_THOLD_G,
         EOH_BIT_G       => EOH_BIT_G,
         ERR_BIT_G       => ERR_BIT_G,
         HEADER_SIZE_G   => HEADER_SIZE_G,
         SHIFT_EN_G      => SHIFT_EN_G,
         AXIS_CONFIG_G   => AXIS_CONFIG_G)    
      port map (
         -- Streaming DMA Interface 
         dmaClk      => dmaClk,
         dmaRst      => dmaRst,
         dmaIbMaster => dmaIbMaster,
         dmaIbSlave  => dmaIbSlave,
         dmaObMaster => dmaObMaster,
         dmaObSlave  => dmaObSlave,
         -- PHY Interface
         phyClk      => phyClk,
         phyRst      => phyRst,
         phyReady    => status.phyReady,
         phyRxd      => phyRxd,
         phyRxc      => phyRxc,
         phyTxd      => phyTxd,
         phyTxc      => phyTxc,
         phyConfig   => config.phyConfig,
         phyStatus   => status.phyStatus);   

   -----------------
   -- 10GBASE-R core
   -----------------
   U_TenGigEthGth7Core : entity work.TenGigEthGth7Core
      port map (
         -- Clocks and Resets
         clk156               => phyClk,
         dclk                 => phyClk,
         txusrclk             => txUsrClk,
         txusrclk2            => txUsrClk2,
         areset               => areset,
         txclk322             => txClk322,
         areset_clk156        => phyRst,
         gttxreset            => status.gtTxRst,
         gtrxreset            => status.gtRxRst,
         txuserrdy            => txUsrRdy,
         reset_counter_done   => status.rstCntDone,
         -- Quad PLL Interface
         qplllock             => status.qplllock,
         qplloutclk           => qplloutclk,
         qplloutrefclk        => qplloutrefclk,
         -- MGT Ports
         txp                  => gtTxP,
         txn                  => gtTxN,
         rxp                  => gtRxP,
         rxn                  => gtRxN,
         -- PHY Interface
         xgmii_txd            => phyTxd,
         xgmii_txc            => phyTxc,
         xgmii_rxd            => phyRxd,
         xgmii_rxc            => phyRxc,
         -- Configuration and Status
         sim_speedup_control  => '0',
         configuration_vector => configurationVector,
         status_vector        => statusVector,
         core_status          => status.core_status,
         tx_resetdone         => status.txRstdone,
         rx_resetdone         => status.rxRstdone,
         signal_detect        => status.sigDet,
         tx_fault             => status.txFault,
         tx_disable           => status.txDisable,
         pma_pmd_type         => config.pma_pmd_type,
         -- MGT Debugging interface
         gt0_eyescanreset     => config.gtEyeScanReset,
         gt0_eyescandataerror => status.gtEyeScanDataError,
         gt0_txbufstatus      => status.gtTxBufStatus,
         gt0_rxbufstatus      => status.gtRxBufStatus,
         gt0_eyescantrigger   => config.gtEyeScanTrigger,
         gt0_rxcdrhold        => config.gtRxCdrHold,
         gt0_txprbsforceerr   => config.gtTxPrbsForceErr,
         gt0_txpolarity       => config.gtTxPolarity,
         gt0_rxpolarity       => config.gtRxPolarity,
         gt0_rxprbserr        => status.gtRxPrbsErr,
         gt0_txpmareset       => config.gtTxPmaReset,
         gt0_rxpmareset       => config.gtRxPmaReset,
         gt0_txresetdone      => status.gtTxResetDone,
         gt0_rxresetdone      => status.gtRxResetDone,
         gt0_rxdfelpmreset    => config.gtRxDfelpmReset,
         gt0_rxlpmen          => config.gtRxlpmen,
         gt0_dmonitorout      => status.gtDmonitorOut,
         gt0_rxrate           => config.gtRxRate,
         gt0_txprecursor      => config.gtTxPreCursor,
         gt0_txpostcursor     => config.gtTxPostCursor,
         gt0_txdiffctrl       => config.gtTxDiffCtrl,
         -- DRP interface
         -- Note: If no arbitration is required on the GT DRP ports 
         -- then connect REQ to GNT and connect other signals i <= o;         
         drp_req              => drpReqGnt,
         drp_gnt              => drpReqGnt,
         drp_den_o            => drpEn,
         drp_dwe_o            => drpWe,
         drp_daddr_o          => drpAddr,
         drp_di_o             => drpDi,
         drp_drdy_o           => drpRdy,
         drp_drpdo_o          => drpDo,
         drp_den_i            => drpEn,
         drp_dwe_i            => drpWe,
         drp_daddr_i          => drpAddr,
         drp_di_i             => drpDi,
         drp_drdy_i           => drpRdy,
         drp_drpdo_i          => drpDo);

   -------------------------------------
   -- 10GBASE-R's Reset Module
   -------------------------------------        
   U_TenGigEthGth7Rst : entity work.TenGigEthGth7Rst
      generic map (
         TPD_G => TPD_G)
      port map (
         -- Clocks and Resets
         extRst     => extRst,
         phyClk     => phyClk,
         phyRst     => phyRst,
         txClk322   => txClk322,
         txUsrClk   => txUsrClk,
         txUsrClk2  => txUsrClk2,
         gtTxRst    => status.gtTxRst,
         gtRxRst    => status.gtRxRst,
         txUsrRdy   => txUsrRdy,
         rstCntDone => status.rstCntDone,
         -- Quad PLL Ports
         qplllock   => status.qplllock,
         qpllRst    => qpllRst); 

   -------------------------------         
   -- Configuration Vector Mapping
   -------------------------------         
   configurationVector(0)              <= config.pma_loopback;
   configurationVector(15)             <= config.pma_reset;
   configurationVector(16)             <= config.global_tx_disable;
   configurationVector(110)            <= config.pcs_loopback;
   configurationVector(111)            <= config.pcs_reset;
   configurationVector(169 downto 112) <= config.test_patt_a_b;
   configurationVector(233 downto 176) <= config.test_patt_a_b;
   configurationVector(240)            <= config.data_patt_sel;
   configurationVector(241)            <= config.test_patt_sel;
   configurationVector(242)            <= config.rx_test_patt_en;
   configurationVector(243)            <= config.tx_test_patt_en;
   configurationVector(244)            <= config.prbs31_tx_en;
   configurationVector(245)            <= config.prbs31_rx_en;
   configurationVector(399 downto 384) <= config.timer_ctrl;
   configurationVector(512)            <= config.set_pma_link_status;
   configurationVector(516)            <= config.set_pcs_link_status;
   configurationVector(518)            <= config.clear_pcs_status2;
   configurationVector(519)            <= config.clear_test_patt_err_count;

   ------------------------
   -- Status Vector Mapping
   ------------------------
   status.pma_link_status         <= statusVector(18);
   status.rx_sig_det              <= statusVector(48);
   status.pcs_rx_link_status      <= statusVector(226);
   status.pcs_rx_locked           <= statusVector(256);
   status.pcs_hiber               <= statusVector(257);
   status.teng_pcs_rx_link_status <= statusVector(268);
   status.pcs_err_block_count     <= statusVector(279 downto 272);
   status.pcs_ber_count           <= statusVector(285 downto 280);
   status.pcs_rx_hiber_lh         <= statusVector(286);
   status.pcs_rx_locked_ll        <= statusVector(287);
   status.pcs_test_patt_err_count <= statusVector(303 downto 288);

   ----------------------
   -- Core Status Mapping
   ----------------------   
   status.phyReady <= status.core_status(0) or config.pcs_loopback;

   --------------------------------     
   -- Configuration/Status Register   
   --------------------------------     
   U_TenGigEthGth7Reg : entity work.TenGigEthGth7Reg
      generic map (
         TPD_G              => TPD_G,
         MAC_ADDR_G         => MAC_ADDR_G,
         STATUS_CNT_WIDTH_G => STATUS_CNT_WIDTH_G,
         AXI_ERROR_RESP_G   => AXI_ERROR_RESP_G)
      port map (
         -- Clocks and resets
         clk            => phyClk,
         rst            => phyRst,
         -- AXI-Lite Register Interface
         axiReadMaster  => mAxiReadMaster,
         axiReadSlave   => mAxiReadSlave,
         axiWriteMaster => mAxiWriteMaster,
         axiWriteSlave  => mAxiWriteSlave,
         -- Configuration and Status Interface
         config         => config,
         status         => status); 

end mapping;