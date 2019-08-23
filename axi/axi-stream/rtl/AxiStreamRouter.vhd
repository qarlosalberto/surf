-------------------------------------------------------------------------------
-- File       : AxiStreamRouter.vhd
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description: AXI stream Router.  
-------------------------------------------------------------------------------
-- This file is part of 'SLAC Firmware Standard Library'.
-- It is subject to the license terms in the LICENSE.txt file found in the 
-- top-level directory of this distribution and at: 
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html. 
-- No part of 'SLAC Firmware Standard Library', including this file, 
-- may be copied, modified, propagated, or distributed except according to 
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

use work.StdRtlPkg.all;
use work.AxiStreamPkg.all;

entity AxiStreamRouter is
   generic (
      TPD_G                 : time     := 1 ns;
      NUM_SLAVES_G          : positive := 1;
      NUM_MASTERS_G         : positive := 1;
      SLAVES_PIPE_STAGES_G  : natural  := 0;
      MASTERS_PIPE_STAGES_G : natural  := 1);
   port (
      -- Clock and reset
      axisClk      : in  sl;
      axisRst      : in  sl;
      -- Routing Configuration
      routeConfig  : in  Slv8Array(NUM_SLAVES_G-1 downto 0);  -- routeConfig[slave=index] = master.index
      blowOffUnmap : in  slv(NUM_SLAVES_G-1 downto 0) := (others => '1');  -- '1' to blow off slave streams that are not routed
      -- Slave Interfaces
      sAxisMasters : in  AxiStreamMasterArray(NUM_SLAVES_G-1 downto 0);
      sAxisSlaves  : out AxiStreamSlaveArray(NUM_SLAVES_G-1 downto 0);
      -- Master Interfaces
      mAxisMasters : out AxiStreamMasterArray(NUM_MASTERS_G-1 downto 0);
      mAxisSlaves  : in  AxiStreamSlaveArray(NUM_MASTERS_G-1 downto 0));
end AxiStreamRouter;

architecture mapping of AxiStreamRouter is

   type RegType is record
      slaves  : AxiStreamSlaveArray(NUM_SLAVES_G-1 downto 0);
      masters : AxiStreamMasterArray(NUM_MASTERS_G-1 downto 0);
   end record RegType;

   constant REG_INIT_C : RegType := (
      slaves  => (others => AXI_STREAM_SLAVE_INIT_C),
      masters => (others => AXI_STREAM_MASTER_INIT_C));

   signal r   : RegType := REG_INIT_C;
   signal rin : RegType;

   signal inputAxisMasters  : AxiStreamMasterArray(NUM_SLAVES_G-1 downto 0);
   signal inputAxisSlaves   : AxiStreamSlaveArray(NUM_SLAVES_G-1 downto 0);
   signal outputAxisMasters : AxiStreamMasterArray(NUM_MASTERS_G-1 downto 0);
   signal outputAxisSlaves  : AxiStreamSlaveArray(NUM_MASTERS_G-1 downto 0);

begin

   GEN_SLAVE : for i in NUM_SLAVES_G-1 downto 0 generate

      U_Input : entity work.AxiStreamPipeline
         generic map (
            TPD_G         => TPD_G,
            PIPE_STAGES_G => SLAVES_PIPE_STAGES_G)
         port map (
            axisClk     => axisClk,
            axisRst     => axisRst,
            sAxisMaster => sAxisMasters(i),
            sAxisSlave  => sAxisSlaves(i),
            mAxisMaster => inputAxisMasters(i),
            mAxisSlave  => inputAxisSlaves(i));

   end generate GEN_SLAVE;

   comb : process (axisRst, blowOffUnmap, inputAxisMasters, outputAxisSlaves,
                   r, routeConfig) is
      variable v        : RegType;
      variable idx      : natural;
      variable tracking : slv(NUM_MASTERS_G-1 downto 0);
   begin
      -- Latch the current value
      v := r;

      -- Reset the tracking map
      tracking := (others => '0');

      -- Flow Control
      v.slaves := (others => AXI_STREAM_SLAVE_INIT_C);
      for i in NUM_MASTERS_G-1 downto 0 loop
         if outputAxisSlaves(i).tReady = '1' then
            v.masters(i).tValid := '0';
         end if;
      end loop;

      for i in NUM_SLAVES_G-1 downto 0 loop

         -- Convert to integer
         idx := conv_integer(routeConfig(i));

         -- Check if bounded by NUM_MASTERS_G range and not mapped yet
         if (idx < NUM_MASTERS_G) and (tracking(idx) = '0') then

            -- Set the flag
            tracking(idx) := '1';

            -- Check if ready to move data
            if (v.masters(idx).tValid = '0') and (inputAxisMasters(i).tValid = '1') then

               -- Accept the data
               v.slaves(i).tReady := '1';

               -- Route AXI streams
               v.masters(idx) := inputAxisMasters(i);

            end if;

         elsif blowOffUnmap(i) = '1' then
            -- Blow off the data
            v.slaves(i).tReady := '1';
         end if;

      end loop;

      -- Outputs
      inputAxisSlaves   <= v.slaves;
      outputAxisMasters <= r.masters;

      -- Reset
      if (axisRst = '1') then
         v := REG_INIT_C;
      end if;

      -- Register the variable for next clock cycle
      rin <= v;

   end process comb;

   seq : process (axisClk) is
   begin
      if (rising_edge(axisClk)) then
         r <= rin after TPD_G;
      end if;
   end process seq;

   GEN_MASTER : for i in NUM_MASTERS_G-1 downto 0 generate

      U_Output : entity work.AxiStreamPipeline
         generic map (
            TPD_G         => TPD_G,
            PIPE_STAGES_G => MASTERS_PIPE_STAGES_G)
         port map (
            axisClk     => axisClk,
            axisRst     => axisRst,
            sAxisMaster => outputAxisMasters(i),
            sAxisSlave  => outputAxisSlaves(i),
            mAxisMaster => mAxisMasters(i),
            mAxisSlave  => mAxisSlaves(i));

   end generate GEN_MASTER;

end mapping;
