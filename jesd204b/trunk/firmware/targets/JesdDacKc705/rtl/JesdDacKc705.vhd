-------------------------------------------------------------------------------
-- Title      : Development board for JESD DAC test
-------------------------------------------------------------------------------
-- File       : JesdDacKc705.vhd
-- Author     : Benjamin Reese <bareese@slac.stanford.edu>
--              Uros Legat <ulegat@slac.stanford.edu>
-- Company    : SLAC National Accelerator Laboratory (Cosylab)
-- Created    : 2015-05-29
-- Last update: 2015-06-04
-- Platform   : Xilinx Kc705 Development platform
--              TI DAC38J82EVM
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
--    Outputs reference clock of 10 MHz on USER Single ended output (to optionally lock all system clocks (LMK chips) to this clock)
--    Outputs JESD clock of 185 MHz on GPIO Single ended output (for debug).
--    Configured for 4-byte operation: GT_WORD_SIZE_C=4
--    To configure for 2-byte operation: GT_WORD_SIZE_C=2, adjust LANE rate, GTX parameters, JESD clock MGMM 
--    LED indicators:
--    - LED0 - Axi Lite clock HB
--    - LED1 - PGP clock HB
--    - LED2 - PGP Rx link ready
--    - LED3 - PGP Tx link ready
--    - LED4 - JESD clock HB
--    - LED5 - JESD QPLL locked
--    - LED6 - JESD nSync signal
--    - LED7 - JESD Data valid
-------------------------------------------------------------------------------
-- Copyright (c) 2015 SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

library UNISIM;
use UNISIM.VCOMPONENTS.all;

use work.StdRtlPkg.all;
use work.AxiLitePkg.all;
use work.AxiStreamPkg.all;
use work.Gtx7CfgPkg.all;
use work.jesd204bpkg.all;
use work.SsiPkg.all;

entity JesdDacKc705 is
   
   generic (
      TPD_G                  : time    := 1 ns;
      SIMULATION_G           : boolean := false;
      -- PGP Config
      PGP_REFCLK_FREQ_G      : real    := 125.0E6;
      PGP_LINE_RATE_G        : real    := 3.125E9;
      -- AXIL Config
      AXIL_CLK_FREQ_G        : real    := 125.0E6;
      -- AXIS Config
      AXIS_CLK_FREQ_G        : real    := 185.0E6;
      AXIS_FIFO_ADDR_WIDTH_G : integer := 9;
      
      --JESD configuration
      -----------------------------------------------------
      -- GTX disconnected 
      SIM_G              : boolean := false;
      -- TRUE  Internal SYSREF
      -- FALSE External SYSREF
      SYSREF_GEN_G       : boolean := false; 
      
      REFCLK_FREQUENCY_G : real     := 185.00E6;
      LINE_RATE_G        : real     := 7.40E9;
      
      -- The JESD module supports values: 1,2,4(four byte GT word only)
      F_G                : positive := 2;
      -- K*F/GT_WORD_SIZE_C has to be integer     
      K_G                : positive := 32;
      -- Number of serial lanes: 1 to 16    
      L_G                : positive := 2);
   port (
      pgpRefClkP : in sl;
      pgpRefClkN : in sl;

      -- PGP MGT signals (SFP)
      pgpGtRxN : in  sl; 
      pgpGtRxP : in  sl;
      pgpGtTxN : out sl;
      pgpGtTxP : out sl;

      -- FMC Signals -- 
      -------------------------------------------------------------------
      -- Signals from clock manager
      fpgaDevClkaP : in sl;             -- FMC-G6-P
      fpgaDevClkaN : in sl;             -- FMC-G7-N
      
      -- JESD synchronisation timing signal (Used in subclass 1 mode)
      -- has to meet setup and hold times of JESD devClk
      -- periodic (period has to be multiple of LMFC clock)
      -- single   (another pulse has to be generated if re-sync needed)      
      fpgaSysRefP  : in sl;             -- FMC-G9-P
      fpgaSysRefN  : in sl;             -- FMC-G10-N

      -- JESD MGT signals 
      adcGtTxP : out slv(1 downto 0);   -- FMC-A4-P -- FMC-B2-P
      adcGtTxN : out slv(1 downto 0);   -- FMC-A3-N -- FMC-B1-N
      adcGtRxP : in  slv(1 downto 0);   
      adcGtRxN : in  slv(1 downto 0);   

      -- JESD receiver requesting sync (Used in all subclass modes)
      -- '1' - synchronisation OK
      -- '0' - synchronisation Not OK - synchronisation request
      syncbP : in sl;                   -- FMC-F10-P
      syncbN : in sl;                   -- FMC-F11-N
      
      -- Debug Signals -- 
      -------------------------------------------------------------------
      -- Onboard LEDs
      leds : out slv(7 downto 0);
      
      -- Digital square wave signal for deterministic latency check 
      rePulseDbg : out slv(1 downto 0); --J46-PIN20 --J46-PIN19
      
      -- Sysref output pin       
      sysRef : out sl;  -- J46-PIN18
      
      -- Out reference clock 10MHz 
      usrClk : out sl;  -- SMA USER CLOCK P
      
      -- Out JESD clock 185MHz
      gpioClk: out sl   -- SMA GPIO CLOCK P
   );
end entity JesdDacKc705;

architecture rtl of JesdDacKc705 is
   -------------------------------------------------------------------------------------------------
   -- PGP constants
   -------------------------------------------------------------------------------------------------
   constant PGP_REFCLK_PERIOD_C : real := 1.0 / PGP_REFCLK_FREQ_G;
   constant PGP_CLK_FREQ_C      : real := PGP_LINE_RATE_G / 20.0;

   -------------------------------------------------------------------------------------------------
   -- JESD constants and signals
   -------------------------------------------------------------------------------------------------
   -- constant REFCLK_FREQUENCY_C : real     := 370.00E6; 
   -- constant REFCLK_FREQUENCY_C : real     := 368.64E6; 
   -- constant REFCLK_FREQUENCY_C : real     := 125.0E6;
   -- constant LINE_RATE_C        : real     := 7.3728E9;
   -- constant LINE_RATE_C        : real     := 7.40E9;
   -- constant LINE_RATE_C        : real     := 2.50E9;
   constant DEVCLK_PERIOD_C    : real     := real(GT_WORD_SIZE_C)*10.0/(LINE_RATE_G);

   signal  s_sysRef : sl;
   signal  s_nsync  : sl;

   -- QPLL config constants
   constant QPLL_CONFIG_C     : Gtx7QPllCfgType := getGtx7QPllCfg(REFCLK_FREQUENCY_G, LINE_RATE_G);   

   -- QPLL
   signal  gtCPllRefClk  : sl; 
   signal  gtCPllLock    : sl; 
   signal  qPllOutClk    : sl; 
   signal  qPllOutRefClk : sl; 
   signal  qPllLock      : sl; 
   signal  qPllRefClkLost: sl; 
   signal  qPllReset     : slv(L_G-1 downto 0); 
   signal  gtQPllReset   : sl;
   

   -------------------------------------------------------------------------------------------------
   -- Clock Signals
   -------------------------------------------------------------------------------------------------
   signal pgpRefClk  : sl;
   signal pgpRefClkG : sl;
   signal axilClk    : sl;
   signal axilClkRst : sl;
   signal pgpClk     : sl;
   signal pgpClkRst  : sl;
   signal pgpMmcmRst : sl;

   signal jesdRefClkDiv2 : sl;
   signal jesdRefClk     : sl;
   signal jesdRefClkG    : sl;
   signal jesdClk        : sl;
   signal jesdClkRst     : sl;
   signal jesdMmcmRst    : sl;

   signal powerOnReset : sl;
   signal masterReset  : sl;
   signal fpgaReload   : sl;
  

   -------------------------------------------------------------------------------------------------
   -- AXI Lite Config and Signals
   -------------------------------------------------------------------------------------------------
   constant NUM_AXI_MASTERS_C : natural := 2;

   constant VERSION_AXIL_INDEX_C : natural              := 0;
   constant JESD_AXIL_INDEX_C    : natural              := 1;

   constant VERSION_AXIL_BASE_ADDR_C : slv(31 downto 0)   := X"00000000";
   constant JESD_AXIL_BASE_ADDR_C    : slv(31 downto 0)   := X"00020000";

   constant AXI_CROSSBAR_MASTERS_CONFIG_C : AxiLiteCrossbarMasterConfigArray(NUM_AXI_MASTERS_C-1 downto 0) := (
      VERSION_AXIL_INDEX_C => (
         baseAddr          => VERSION_AXIL_BASE_ADDR_C,
         addrBits          => 12,
         connectivity      => X"0001"),
      JESD_AXIL_INDEX_C    => (
         baseAddr          => JESD_AXIL_BASE_ADDR_C,
         addrBits          => 12,
         connectivity      => X"0001"));

   signal extAxilWriteMaster : AxiLiteWriteMasterType;
   signal extAxilWriteSlave  : AxiLiteWriteSlaveType;
   signal extAxilReadMaster  : AxiLiteReadMasterType;
   signal extAxilReadSlave   : AxiLiteReadSlaveType;

   signal locAxilWriteMasters : AxiLiteWriteMasterArray(NUM_AXI_MASTERS_C-1 downto 0);
   signal locAxilWriteSlaves  : AxiLiteWriteSlaveArray(NUM_AXI_MASTERS_C-1 downto 0);
   signal locAxilReadMasters  : AxiLiteReadMasterArray(NUM_AXI_MASTERS_C-1 downto 0);
   signal locAxilReadSlaves   : AxiLiteReadSlaveArray(NUM_AXI_MASTERS_C-1 downto 0);

   -------------------------------------------------------------------------------------------------
   -- PGP Signals and Virtual Channels
   -------------------------------------------------------------------------------------------------
   constant JESD_SSI_CONFIG_C : AxiStreamConfigType := ssiAxiStreamConfig(GT_WORD_SIZE_C, TKEEP_COMP_C);

   signal axisTxMasters : AxiStreamMasterArray(1 downto 0);
   signal axisTxSlaves  : AxiStreamSlaveArray(1 downto 0);
   signal axisTxCtrl    : AxiStreamCtrlArray(1 downto 0);
   
   -------------------------------------------------------------------------------------------------
   -- PGP Signals and Virtual Channels
   -------------------------------------------------------------------------------------------------
   signal s_usrClk : sl;   
   signal s_usrRst : sl;

   -------------------------------------------------------------------------------------------------
   -- Debug
   -------------------------------------------------------------------------------------------------   
   signal s_syncAllLED  : sl;
   signal s_validAllLED : sl;
   signal s_rePulse     : slv(L_G-1 downto 0);

begin

   -------------------------------------------------------------------------------------------------
   -- ADC EVM Out reference clock (10.00 MHz)
   -------------------------------------------------------------------------------------------------
      ClockManager7_OUT : entity work.ClockManager7
      generic map (
         TPD_G              => TPD_G,
         TYPE_G             => "MMCM",
         INPUT_BUFG_G       => false,
         FB_BUFG_G          => true,
         NUM_CLOCKS_G       => 1,
         BANDWIDTH_G        => "HIGH",
         CLKIN_PERIOD_G     => 8.0,      --(10.0MHz)  (156.25MHz)(368.64MHz) (61.44 MHz) (370MHz)
         DIVCLK_DIVIDE_G    => 1,        --1,         --4,         --6,        --5,        --5,
         CLKFBOUT_MULT_F_G  => 8.000,    --8.0,       --55,        --50.875,   --47.000,   --37.000,
         CLKOUT0_DIVIDE_F_G => 100.0,    --100.0,     --11,        --2.875,    --19.125,   --2.5
         CLKOUT0_RST_HOLD_G => 16)
   port map (
      clkIn     => pgpRefClkG,
      rstIn     => pgpMmcmRst,
      clkOut(0) => s_usrClk,
      rstOut(0) => s_usrRst);
    
   -------------------------------------------------------------------------------------------------
   -- Bring in gt reference clocks
   -------------------------------------------------------------------------------------------------
   IBUFDS_GTE2_GTREFCLK125 : IBUFDS_GTE2
      port map (
         I   => pgpRefClkP,
         IB  => pgpRefClkN,
         CEB => '0',
         O   => pgpRefClk);

   GTREFCLK125_BUFG : BUFG
      port map (
         I => pgpRefClk,
         O => pgpRefClkG);

   PwrUpRst_1 : entity work.PwrUpRst
      generic map (
         TPD_G          => TPD_G,
         SIM_SPEEDUP_G  => SIMULATION_G,
         IN_POLARITY_G  => '1',
         OUT_POLARITY_G => '1')
      port map (
         clk    => pgpRefClkG,
         rstOut => powerOnReset);

   -------------------------------------------------------------------------------------------------
   -- Create global clocks from gt ref clocks
   -------------------------------------------------------------------------------------------------
   pgpMmcmRst <= masterReset or powerOnReset;

   ClockManager7_PGP : entity work.ClockManager7
      generic map (
         TPD_G              => TPD_G,
         TYPE_G             => "MMCM",
         INPUT_BUFG_G       => false,
         FB_BUFG_G          => true,
         NUM_CLOCKS_G       => 2,
         BANDWIDTH_G        => "OPTIMIZED",
         CLKIN_PERIOD_G     => PGP_REFCLK_PERIOD_C*1.0E9,
         DIVCLK_DIVIDE_G    => 1,
         CLKFBOUT_MULT_F_G  => 7.5,
         CLKOUT0_DIVIDE_F_G => 7.5,
         CLKOUT0_RST_HOLD_G => 16,
         CLKOUT1_DIVIDE_G   => 6,
         CLKOUT1_RST_HOLD_G => 16)
      port map (
         clkIn     => pgpRefClkG,
         rstIn     => pgpMmcmRst,
         clkOut(0) => axilClk,
         clkOut(1) => pgpClk,
         rstOut(0) => axilClkRst,
         rstOut(1) => pgpClkRst);

   -------------------------------------------------------------------------------------------------
   -- PGP Interface 
   -------------------------------------------------------------------------------------------------
   PgpFrontEnd_1 : entity work.PgpFrontEnd
      generic map (
         TPD_G                  => TPD_G,
         SIMULATION_G           => SIMULATION_G,
         PGP_REFCLK_FREQ_G      => PGP_REFCLK_FREQ_G,
         PGP_LINE_RATE_G        => PGP_LINE_RATE_G,
         AXIL_CLK_FREQ_G        => AXIL_CLK_FREQ_G,
         AXIS_CLK_FREQ_G        => AXIS_CLK_FREQ_G,
         AXIS_FIFO_ADDR_WIDTH_G => AXIS_FIFO_ADDR_WIDTH_G,
         AXIS_CONFIG_G          => JESD_SSI_CONFIG_C)
      port map (
         pgpRefClk       => pgpRefClk,
         pgpClk          => pgpClk,
         pgpClkRst       => pgpClkRst,
         pgpGtRxN        => pgpGtRxN,
         pgpGtRxP        => pgpGtRxP,
         pgpGtTxN        => pgpGtTxN,
         pgpGtTxP        => pgpGtTxP,
         axilClk         => axilClk,
         axilClkRst      => axilClkRst,
         axilWriteMaster => extAxilWriteMaster,
         axilWriteSlave  => extAxilWriteSlave,
         axilReadMaster  => extAxilReadMaster,
         axilReadSlave   => extAxilReadSlave,
         axisClk         => jesdClk,
         axisClkRst      => jesdClkRst,
         axisTxMasters   => axisTxMasters, -- Disconnected
         axisTxSlaves    => axisTxSlaves,  -- Disconnected
         axisTxCtrl      => axisTxCtrl,
         leds            => leds(3 downto 2));

   -------------------------------------------------------------------------------------------------
   -- Top Axi Crossbar
   -------------------------------------------------------------------------------------------------
   TopAxiCrossbar : entity work.AxiLiteCrossbar
      generic map (
         TPD_G              => TPD_G,
         NUM_SLAVE_SLOTS_G  => 1,
         NUM_MASTER_SLOTS_G => NUM_AXI_MASTERS_C,
         MASTERS_CONFIG_G   => AXI_CROSSBAR_MASTERS_CONFIG_C)
      port map (
         axiClk              => axilClk,
         axiClkRst           => axilClkRst,
         sAxiWriteMasters(0) => extAxilWriteMaster,
         sAxiWriteSlaves(0)  => extAxilWriteSlave,
         sAxiReadMasters(0)  => extAxilReadMaster,
         sAxiReadSlaves(0)   => extAxilReadSlave,
         mAxiWriteMasters    => locAxilWriteMasters,
         mAxiWriteSlaves     => locAxilWriteSlaves,
         mAxiReadMasters     => locAxilReadMasters,
         mAxiReadSlaves      => locAxilReadSlaves);

   -------------------------------------------------------------------------------------------------
   -- Put version info on AXI Bus
   -------------------------------------------------------------------------------------------------
   AxiVersion_1 : entity work.AxiVersion
      generic map (
         TPD_G            => TPD_G,
         EN_DEVICE_DNA_G  => true,
         EN_DS2411_G      => false,
         EN_ICAP_G        => true,
         AUTO_RELOAD_EN_G => false)
      port map (
         axiClk         => axilClk,
         axiRst         => axilClkRst,
         axiReadMaster  => locAxilReadMasters(VERSION_AXIL_INDEX_C),
         axiReadSlave   => locAxilReadSlaves(VERSION_AXIL_INDEX_C),
         axiWriteMaster => locAxilWriteMasters(VERSION_AXIL_INDEX_C),
         axiWriteSlave  => locAxilWriteSlaves(VERSION_AXIL_INDEX_C),
         masterReset    => masterReset);


   -------------------------------------------------------------------------------------------------
   -- JESD Clocking
   -------------------------------------------------------------------------------------------------
   IBUFDS_GTE2_FPGADEVCLKA : IBUFDS_GTE2
      port map (
         I     => fpgaDevClkaP,
         IB    => fpgaDevClkaN,
         CEB   => '0',
         ODIV2 => jesdRefClkDiv2,
         O     => jesdRefClk          
   );
     
   JESDREFCLK_BUFG : BUFG
      port map (
         I => jesdRefClk, -- JESD clock = refclk = 185 MHz
         O => jesdRefClkG);

   jesdMmcmRst <= powerOnReset or masterReset;

   ClockManager7_JESD : entity work.ClockManager7
      generic map (
         TPD_G              => TPD_G,
         TYPE_G             => "MMCM",
         INPUT_BUFG_G       => false,
         FB_BUFG_G          => true,
         NUM_CLOCKS_G       => 1,
         BANDWIDTH_G        => "OPTIMIZED",
         CLKIN_PERIOD_G     => DEVCLK_PERIOD_C*1.0E9,
         DIVCLK_DIVIDE_G    => 1,
         CLKFBOUT_MULT_F_G  => 5.375,
         CLKOUT0_DIVIDE_F_G => 5.375,
         CLKOUT0_RST_HOLD_G => 16)
      port map (
         clkIn     => jesdRefClkG,
         rstIn     => jesdMmcmRst,
         clkOut(0) => jesdClk,
         rstOut(0) => jesdClkRst);
            
   -------------------------------------------------------------------------------------------------
   -- QPLL for JESD MGTs
   ------------------------------------------------------------------------------------------------- 
   Gtx7QuadPll_INST: entity work.Gtx7QuadPll
   generic map (
      TPD_G               => TPD_G,
      QPLL_CFG_G          => x"06801C1", -- TODO check
      QPLL_REFCLK_SEL_G   => "001",      -- Should be ok
      QPLL_FBDIV_G        => QPLL_CONFIG_C.QPLL_FBDIV_G,      -- use getGtx7QPllCfg to set b'0000110000'
      QPLL_FBDIV_RATIO_G  => QPLL_CONFIG_C.QPLL_FBDIV_RATIO_G,-- use getGtx7QPllCfg to set '1'
      QPLL_REFCLK_DIV_G   => QPLL_CONFIG_C.QPLL_REFCLK_DIV_G  -- use getGtx7QPllCfg to set '1'
   )
   port map (
      qPllRefClk     => jesdRefClk, -- Reference clock directly from the input
      qPllOutClk     => qPllOutClk,
      qPllOutRefClk  => qPllOutRefClk,
      qPllLock       => qPllLock,
      qPllLockDetClk => pgpClk,
      qPllRefClkLost => qPllRefClkLost,
      qPllPowerDown  => '0',
      qPllReset      => qPllReset(0)
   );      
  
   -------------------------------------------------------------------------------------------------
   -- JESD Tx block
   -------------------------------------------------------------------------------------------------   
   Jesd204bTxGtx7_INST: entity work.Jesd204bTxGtx7
   generic map (
      TPD_G                 => TPD_G,
     
      -- GTX disconnected
      SIM_G      =>  SIM_G,
      -- Internal SYSREF SYSREF_GEN_G= TRUE else 
      -- External SYSREF
      SYSREF_GEN_G =>  SYSREF_GEN_G,    
      
      -- CPLL Configurations (not used)
      CPLL_FBDIV_G          => 4,  -- use getGtx7CPllCfg to set
      CPLL_FBDIV_45_G       => 4,  -- use getGtx7CPllCfg to set
      CPLL_REFCLK_DIV_G     => 1,  -- use getGtx7CPllCfg to set
      
      RXOUT_DIV_G           => QPLL_CONFIG_C.OUT_DIV_G,  -- use getGtx7QPllCfg to set
      RX_CLK25_DIV_G        => QPLL_CONFIG_C.CLK25_DIV_G,-- use getGtx7QPllCfg to set,
                                                       
      -- Configure PLL sources
      TX_PLL_G              =>  "QPLL", -- "QPLL" or "CPLL"
      RX_PLL_G              =>  "QPLL", -- "QPLL" or "CPLL"
      
      -- MGT Configurations (USE Xilinx Coregen to set those, depending on the clocks)
      PMA_RSV_G             =>  x"001E7080",            -- Values from coregen     
      RX_OS_CFG_G           =>  "0000010000000",        -- Values from coregen 
      RXCDR_CFG_G           =>  x"03000023ff10400020",  -- Values from coregen  
      RXDFEXYDEN_G          =>  '1',                    -- Values from coregen 
      RX_DFE_KL_CFG2_G      =>  x"301148AC",            -- Values from coregen 
         
      TXOUT_DIV_G           =>   QPLL_CONFIG_C.OUT_DIV_G,
      TX_CLK25_DIV_G        =>   QPLL_CONFIG_C.CLK25_DIV_G,
      TX_BUF_EN_G           =>   true,
      TX_OUTCLK_SRC_G       =>   "OUTCLKPMA",
      TX_DLY_BYPASS_G       =>   '1',
      TX_PHASE_ALIGN_G      =>   "NONE",
      TX_BUF_ADDR_MODE_G    =>   "FULL",
           
      -- AXI
      AXI_ERROR_RESP_G      => AXI_RESP_SLVERR_C,
      
      -- JESD
      F_G                => F_G,
      K_G                => K_G,
      L_G                => L_G)
   port map (
      qPllRefClkIn      => qPllOutRefClk,
      qPllClkIn         => qPllOutClk,
      qPllLockIn        => qPllLock,
      qPllRefClkLostIn  => qPllRefClkLost,
      qPllResetOut      => qPllReset,
      
      gtTxP(0)          => adcGtTxP(0),
      gtTxP(1)          => adcGtTxP(1),      
      gtTxN(0)          => adcGtTxN(0),
      gtTxN(1)          => adcGtTxN(1),  
      gtRxP(0)          => adcGtRxP(0),
      gtRxP(1)          => adcGtRxP(1),      
      gtRxN(0)          => adcGtRxN(0),
      gtRxN(1)          => adcGtRxN(1),
      
      stableClk         => jesdRefClkG, -- Stable because it is never reset
      devClk_i          => jesdClk, -- both same
      devClk2_i         => jesdClk, -- both same
      devRst_i          => jesdClkRst,
      axiClk            => axilClk,
      axiRst            => axilClkRst,
      
      axilReadMasterTx  => locAxilReadMasters(JESD_AXIL_INDEX_C),
      axilReadSlaveTx   => locAxilReadSlaves(JESD_AXIL_INDEX_C),
      axilWriteMasterTx => locAxilWriteMasters(JESD_AXIL_INDEX_C),
      axilWriteSlaveTx  => locAxilWriteSlaves(JESD_AXIL_INDEX_C),
      
      --Currently no AXI stream input
      txAxisMasterArr_i  => (L_G-1 downto 0 => AXI_STREAM_MASTER_INIT_C),
      txAxisSlaveArr_o   => open,
      
      -- External sample data input
      extSampleDataArray_i => (L_G-1 downto 0 => (RX_STAT_WIDTH_C-1 downto 0 => '0')),
      
      pulse_o    => s_rePulse,
      
      leds_o(0)         => s_syncAllLED, -- (0) Sync
      leds_o(1)         => s_validAllLED,-- (1) Data_valid
      
      sysRef_i          => s_sysRef,
      sysRef_o          => open, -- TODO Add internal sysref GEN output          
      nSync_i           => s_nSync
   );
   
   ----------------------------------------------------------------
   -- Get sync and sysref from differential io buffer
   ----------------------------------------------------------------
   IBUFDS_rsysref_inst : IBUFDS
   generic map (
      DIFF_TERM => FALSE,
      IBUF_LOW_PWR => TRUE,
      IOSTANDARD => "DEFAULT")
   port map (
      I  => fpgaSysRefP,
      IB => fpgaSysRefN,
      O  => s_sysRef
   );
   
   sysRef <= s_sysRef;
   
   IBUFDS_nsync_inst : IBUFDS
   generic map (
      DIFF_TERM => FALSE,
      IBUF_LOW_PWR => TRUE,
      IOSTANDARD => "DEFAULT")
   port map (
      O =>  s_nSync,
      I =>  syncbP, 
      IB => syncbN
   );

   -------------------------------------------------------------------------------------------------
   -- LED Test Outputs
   -------------------------------------------------------------------------------------------------
   Heartbeat_axilClk : entity work.Heartbeat
      generic map (
         TPD_G        => TPD_G,
         PERIOD_IN_G  => 8.0E-9,
         PERIOD_OUT_G => 1.0)
      port map (
         clk => axilClk,
         o   => leds(0));

   Heartbeat_pgpClk : entity work.Heartbeat
      generic map (
         TPD_G        => TPD_G,
         PERIOD_IN_G  => 6.4E-9,
         PERIOD_OUT_G => 1.0)
      port map (
         clk => pgpClk,
         o   => leds(1));
         
   Heartbeat_jesdclk : entity work.Heartbeat
      generic map (
         TPD_G        => TPD_G,
         PERIOD_IN_G  => 5.425E-9,
         PERIOD_OUT_G => 1.0)
      port map (
         clk => jesdClk,
         o   => leds(4));
         
   leds(5) <= qPllLock;
   leds(6) <= s_syncAllLED;
   leds(7) <= s_validAllLED; 
   
   -- Output user clock for single ended reference  
   UserClkBufSingle_INST: entity work.ClkOutBufSingle
   generic map (
      XIL_DEVICE_G   => "7SERIES",
      RST_POLARITY_G => '1',
      INVERT_G       => false)
   port map (
      clkIn       => s_usrClk,
      rstIn       => s_usrRst,
      clkOut      => usrClk);

   -- Output JESD clk for debug
   GPioClkBufSingle_INST: entity work.ClkOutBufSingle
   generic map (
      XIL_DEVICE_G   => "7SERIES",
      RST_POLARITY_G => '1',
      INVERT_G       => false)
   port map (
      clkIn  => jesdClk,
      rstIn  => jesdClkRst,
      clkOut => gpioClk);
      
      
   -- Debug output pins
   OBUF_rePulse_0_inst : OBUF
   port map (
      I => s_rePulse(0),
      O => rePulseDbg(0)
   );
   
   -- Debug output pins
   OBUF_rePulse_1_inst : OBUF
   port map (
      I => s_rePulse(1),
      O => rePulseDbg(1)
   );   
      
   
end architecture rtl;