-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- Company    : SLAC National Accelerator Laboratory
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- This file is part of SURF. It is subject to
-- the license terms in the LICENSE.txt file found in the top-level directory
-- of this distribution and at:
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
-- No part of SURF, including this file, may be
-- copied, modified, propagated, or distributed except according to the terms
-- contained in the LICENSE.txt file.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library surf;
use surf.StdRtlPkg.all;
use surf.AxiStreamPkg.all;
use surf.Pgp3Pkg.all;

entity Pgp3TxLiteWrapper is

   generic (
      TPD_G : time := 1 ns);
   port (
      clk        : in  sl;
      rst        : in  sl;
      txReady    : out sl;
      txValid    : in  sl;
      txData     : in  slv(63 downto 0);
      txFrame    : in  sl;
      txSof      : in  sl;
      txEof      : in  sl;
      txEofe     : in  sl;
      phyTxValid : out sl;
      phyTxData  : out slv(65 downto 0));

end entity Pgp3TxLiteWrapper;

architecture rtl of Pgp3TxLiteWrapper is

   signal pgpTxIn : Pgp3TxInType := (
      disable      => '0',
      flowCntlDis  => '1',
      resetTx      => '0',
      skpInterval  => (others => '0'),  -- No skips
      opCodeEn     => '0',
      opCodeNumber => (others => '0'),
      opCodeData   => (others => '0'),
      locData      => (others => '0'));

   signal pgpTxMaster : AxiStreamMasterType := AXI_STREAM_MASTER_INIT_C;
   signal pgpTxSlave  : AxiStreamSlaveType;

begin

   txReady                        <= pgpTxSlave.tReady;
   pgpTxMaster.tValid             <= txValid;
   pgpTxMaster.tData(63 downto 0) <= txData;
   pgpTxMaster.tKeep(7 downto 0)  <= X"FF";
   pgpTxMaster.tUser(1)           <= txSof;
   pgpTxMaster.tUser(14)          <= txEofe;
   pgpTxMaster.tLast              <= txEof;


   U_Pgp3TxLite_1 : entity surf.Pgp3TxLite
      generic map (
         TPD_G    => TPD_G,
         NUM_VC_G => 1)
      port map (
         pgpTxClk        => clk,                       -- [in]
         pgpTxRst        => rst,                       -- [in]
         pgpTxIn         => pgpTxIn,                   -- [in]
         pgpTxOut        => open,                      -- [out]
         pgpTxActive     => txFrame,                   -- [in]
         pgpTxMasters(0) => pgpTxMaster,               -- [in]
         pgpTxSlaves(0)  => pgpTxSlave,                -- [out]
--          locRxFifoCtrl  => locRxFifoCtrl,   -- [in]
--          locRxLinkReady => locRxLinkReady,  -- [in]
--          remRxFifoCtrl  => remRxFifoCtrl,   -- [in]
--          remRxLinkReady => remRxLinkReady,  -- [in]
         phyTxActive     => '1',                       -- [in]
         phyTxReady      => '1',                       -- [in]
         phyTxValid      => phyTxValid,                -- [out]
         phyTxStart      => open,                      -- [out]
         phyTxData       => phyTxData(63 downto 0),    -- [out]
         phyTxHeader     => phyTxData(65 downto 64));  -- [out]

end architecture rtl;
