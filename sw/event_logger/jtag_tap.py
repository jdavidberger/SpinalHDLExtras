#!/usr/bin/env python3
from pyftdi.jtag import JtagEngine, JtagTool
from pyftdi.bits import BitSequence
from pyftdi.ftdi import Ftdi

import struct
import time

class Jtag:
    def __init__(self, url='ftdi://ftdi:232h/1', clk=6E6):
        """Initialize FTDI JTAG engine."""
        self.jtag = JtagEngine(frequency=12e6)
        self.tool = JtagTool(self.jtag)

        self.jtag._ctrl._ftdi.open_mpsse_from_url(url, direction=0x1b, frequency=12e6)
        # FTDI requires to initialize all GPIOs before MPSSE kicks in
        cmd = bytearray((Ftdi.SET_BITS_LOW, 0x08, 0x1b))
        self.jtag._ctrl._ftdi.write_data(cmd)
        self.jtag._ctrl._ftdi.timeouts = (10000, 10000)
        #self.jtag.configure(url)
        self.reset()

    # ---------------- Core TAP ops ----------------
    def reset(self):
        """Force TAP into Test-Logic-Reset."""
        self.jtag.reset()

    def query_device_id_count(self):
        self.reset()

        self.jtag.change_state('shift_ir')
        self.jtag.shift_and_update_register(BitSequence('1' * 32))
        self.jtag.change_state('shift_dr')
        self.jtag.shift_register(BitSequence('0' * 32))
        bypass = self.jtag.shift_and_update_register(BitSequence(('1' * 32)))
        self.reset()

        for i in range(0, len(bypass)):
            if bypass[i] == 1:
                return i

        return -1


    # ---------------- Utilities ----------------
    def query_device_ids(self) -> list[int]:
        devCnt = self.query_device_id_count()
        self.jtag.change_state('shift_dr')
        bits = self.jtag.shift_and_update_register(BitSequence('0' * (devCnt * 32)))
        return struct.unpack('<' + (devCnt * 'I'), bits.reverse().tobytes(True, True))

    def _find_ir_shift(self):
        devices = self.query_device_ids()

        ir_lengths = []
        logger_idx = -1
        for idx, dev in enumerate(devices):
            if dev == 0x010003d1:
                ir_lengths.append(8)
                logger_idx = idx
            else:
                ir_lengths.append(5)

        cnt = 0
        idata = 0
        for ridx, dev in reversed(list(enumerate(devices))):
            if ridx == logger_idx:
                idata = (idata << 8) | 0x24
                cnt += 8
            else:
                idata = (idata << ir_lengths[ridx]) | ((1 << ir_lengths[ridx])-1)
                cnt += ir_lengths[ridx]

        return BitSequence(idata, length = cnt)

    def capture_event_stream(self):
        ir_shift = self._find_ir_shift()
        self.jtag.change_state('shift_ir')
        self.jtag.shift_and_update_register(ir_shift)
        self.jtag.change_state('shift_dr')

        buffer_size = 256 # 8 * 4096
        buffer = BitSequence('0' * buffer_size)

        counter = 0
        #d32 = bytearray(12)
        d32 = [0, 0, 0]
        while True:
            out_buffer = self.jtag.shift_register(buffer).sequence()

            for b in out_buffer:
                if counter == 0:
                    if b:
                        counter = 96
                if counter != 0:
                    rev_c = 96 - counter
                    counter = counter - 1
                    d32[rev_c // 32] |= (b << (rev_c % 32))
                    if counter == 0:
                        yield d32
                        d32 = [0, 0, 0]


    def close(self):
        self.jtag.close()

# ---------------------------------------------------------------------
# Test main
# ---------------------------------------------------------------------
if __name__ == '__main__':
    import sys
    url = sys.argv[1] if len(sys.argv) > 1 else 'ftdi://ftdi:0x6010/1'

    print(f"Opening JTAG interface at {url}")
    jtag = Jtag(url)

    print_time = time.time()
    row_idx = 0
    try:
        for event in jtag.capture_event_stream():
            row_idx = row_idx + 1
            if print_time + 1 < time.time():
                print_time = time.time()
                print(f"Captured {row_idx} per second")
                row_idx = 0
            #print(event)

    finally:
        jtag.close()
        print("Closed JTAG interface")
