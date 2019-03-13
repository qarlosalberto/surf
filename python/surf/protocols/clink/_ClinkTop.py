#!/usr/bin/env python
#-----------------------------------------------------------------------------
# Title      : PyRogue CameraLink module
#-----------------------------------------------------------------------------
# File       : ClinkTop.py
# Created    : 2017-11-21
#-----------------------------------------------------------------------------
# Description:
# PyRogue CameraLink module
#-----------------------------------------------------------------------------
# This file is part of the rogue software platform. It is subject to
# the license terms in the LICENSE.txt file found in the top-level directory
# of this distribution and at:
#    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
# No part of the rogue software platform, including this file, may be
# copied, modified, propagated, or distributed except according to the terms
# contained in the LICENSE.txt file.
#-----------------------------------------------------------------------------

import pyrogue as pr
import surf.protocols.clink

class ClinkTop(pr.Device):
    def __init__(   self,       
            name        = "ClinkTop",
            description = "CameraLink module",
            serialA     = None,
            serialB     = None,
            **kwargs):
        super().__init__(name=name, description=description, **kwargs) 

        ##############################
        # Variables
        ##############################

        self.add(pr.RemoteVariable(    
            name         = "ChanCount",
            description  = "Supported channels",
            offset       =  0x00,
            bitSize      =  4,
            bitOffset    =  0x00,
            base         = pr.UInt,
            mode         = "RO",
        ))

        self.add(pr.RemoteCommand(   
            name         = "ResetPll",
            description  = "Camera link channel PLL reset",
            offset       =  0x04,
            bitSize      =  1,
            bitOffset    =  0,
            function     = pr.BaseCommand.toggle,
        ))   

        self.add(pr.RemoteCommand(   
            name         = "ResetFsm",
            description  = "Camera link channel FSM reset",
            offset       =  0x04,
            bitSize      =  1,
            bitOffset    =  1,
            function     = pr.BaseCommand.toggle,
        ))           
        
        self.add(pr.RemoteVariable(    
            name         = "LinkLockedA",
            description  = "Camera link channel locked status",
            offset       =  0x10,
            bitSize      =  1,
            bitOffset    =  0,
            base         = pr.Bool,
            pollInterval = 1,
            mode         = "RO",
        ))

        self.add(pr.RemoteVariable(    
            name         = "LinkLockedB",
            description  = "Camera link channel locked status",
            offset       =  0x10,
            bitSize      =  1,
            bitOffset    =  1,
            base         = pr.Bool,
            pollInterval = 1,
            mode         = "RO",
        ))

        self.add(pr.RemoteVariable(    
            name         = "LinkLockedC",
            description  = "Camera link channel locked status",
            offset       =  0x10,
            bitSize      =  1,
            bitOffset    =  2,
            base         = pr.Bool,
            pollInterval = 1,
            mode         = "RO",
        ))

        self.add(pr.RemoteVariable(    
            name         = "ShiftCountA",
            description  = "Shift count for channel",
            offset       =  0x14,
            bitSize      =  3,
            bitOffset    =  0,
            base         = pr.UInt,
            pollInterval = 1,
            mode         = "RO",
        ))

        self.add(pr.RemoteVariable(    
            name         = "ShiftCountB",
            description  = "Shift count for channel",
            offset       =  0x14,
            bitSize      =  3,
            bitOffset    =  8,
            base         = pr.UInt,
            pollInterval = 1,
            mode         = "RO",
        ))

        self.add(pr.RemoteVariable(    
            name         = "ShiftCountC",
            description  = "Shift count for channel",
            offset       =  0x14,
            bitSize      =  3,
            bitOffset    =  16,
            base         = pr.UInt,
            pollInterval = 1,
            mode         = "RO",
        ))

        self.add(pr.RemoteVariable(    
            name         = "DelayA",
            description  = "Precision delay for channel A",
            offset       =  0x18,
            bitSize      =  5,
            bitOffset    =  0,
            base         = pr.UInt,
            pollInterval = 1,
            mode         = "RO",
        ))

        self.add(pr.RemoteVariable(    
            name         = "DelayB",
            description  = "Precision delay for channel B",
            offset       =  0x18,
            bitSize      =  5,
            bitOffset    =  8,
            base         = pr.UInt,
            pollInterval = 1,
            mode         = "RO",
        ))

        self.add(pr.RemoteVariable(    
            name         = "DelayC",
            description  = "Precision delay for channel C",
            offset       =  0x18,
            bitSize      =  5,
            bitOffset    =  16,
            base         = pr.UInt,
            pollInterval = 1,
            mode         = "RO",
        ))

        self.add(surf.protocols.clink.ClinkChannel( name = "ChannelA", serial=serialA, offset=0x100))
        self.add(surf.protocols.clink.ClinkChannel( name = "ChannelB", serial=serialB, offset=0x200))


    def writeBlocks(self, force=False, recurse=True, variable=None, checkEach=False):
        """
        Write all of the blocks held by this Device to memory
        """
        self._log.debug(f'Calling {self.path}._writeBlocks')

        checkEach = checkEach or self.forceCheckEach

        # Process local blocks.
        if variable is not None:
            for b in self._getBlocks(variable):
                if (force or b.stale):                
                    b.startTransaction(rim.Write, check=checkEach)

        else:
            for block in self._blocks:
                if (force or block.stale) and block.bulkEn:
                    block.startTransaction(rim.Write, check=checkEach)

            if recurse:
                for key,value in self.devices.items():
                    value.writeBlocks(force=force, recurse=True, checkEach=checkEach)
        
        # Execute FSM reset after loading YAML
        self.ResetFsm()