package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinal.lib._

// https://www.latticesemi.com/en/Support/PartNumberReferenceGuide/AllFamilies

package object IO {
  type BB = spinal.lib.blackbox.lattice.ecp5.BB
}

case class ODDRX1() extends BlackBox {
  val io = new Bundle {
    val D0 = in(Bool())
    val D1 = in(Bool())
    val SCLK = in(Bool())
    val RST = in(Bool())

    val Q = out(Bool())

    val D = Seq(D0, D1)
  }
  noIoPrefix()
}

case class ODDRX2() extends BlackBox {
  val io = new Bundle {
    val D0 = in(Bool())
    val D1 = in(Bool())
    val D2 = in(Bool())
    val D3 = in(Bool())

    val D = Seq(D0, D1, D2, D3)

    val ECLK = in(Bool())
    val SCLK = in(Bool())
    val RST = in(Bool())

    val Q = out(Bool())
  }
  noIoPrefix()
  mapCurrentClockDomain(io.SCLK, io.RST)
}

case class ODDRX4() extends BlackBox {
  val io = new Bundle {
    val D0, D1, D2, D3, D4, D5, D6, D7 = in(Bool())

    val D = Seq(D0, D1, D2, D3, D4, D5, D6, D7)

    val ECLK = in(Bool())
    val SCLK = in(Bool())
    val RST = in(Bool())

    val Q = out(Bool())
  }
  noIoPrefix()
  mapCurrentClockDomain(io.SCLK, io.RST)
}

case class ODDRX5() extends BlackBox {
  val io = new Bundle {
    val D0, D1, D2, D3, D4, D5, D6, D7, D8, D9 = in(Bool())

    val D = Seq(D0, D1, D2, D3, D4, D5, D6, D7, D8, D9)

    val ECLK = in(Bool())
    val SCLK = in(Bool())
    val RST = in(Bool())

    val Q = out(Bool())
  }
  noIoPrefix()
  mapCurrentClockDomain(io.SCLK, io.RST)
}

case class ODDR71() extends BlackBox {
  val io = new Bundle {
    val D0, D1, D2, D3, D4, D5, D6 = in(Bool())

    val D = Seq(D0, D1, D2, D3, D4, D5, D6)

    val ECLK = in(Bool())
    val SCLK = in(Bool())
    val RST = in(Bool())

    val Q = out(Bool())
  }
  noIoPrefix()
  mapCurrentClockDomain(io.SCLK, io.RST)
}

case class IDDRX1() extends BlackBox {
  val io = new Bundle {
    val D = in(Bool())

    // Positive edge
    val Q0 = out(Bool())

    // Negative edge
    val Q1 = out(Bool())

    val Q = Seq(Q0, Q1)

    val SCLK = in(Bool())
    val RST = in(Bool())
  }
  noIoPrefix()
  mapCurrentClockDomain(io.SCLK, io.RST)
}


case class IDDRX2() extends BlackBox {
  val io = new Bundle {
    val D = in(Bool())

    // Positive edge
    val Q0 = out(Bool())
    val Q1 = out(Bool())
    val Q2 = out(Bool())
    val Q3 = out(Bool())

    val Q = Seq(Q0, Q1, Q2, Q3)

    // Shifts word alignment by one bit.
    val ALIGNWD = in(Bool()) default(False)
    val SCLK = in(Bool())
    val ECLK = in(Bool())
    val RST = in(Bool())
  }
  noIoPrefix()
  mapCurrentClockDomain(io.SCLK, io.RST)
}

case class IDDRX4() extends BlackBox {
  val io = new Bundle {
    val D = in(Bool())

    // Positive edge
    val Q0 = out(Bool())
    val Q1 = out(Bool())
    val Q2 = out(Bool())
    val Q3 = out(Bool())
    val Q4 = out(Bool())
    val Q5 = out(Bool())
    val Q6 = out(Bool())
    val Q7 = out(Bool())

    val Q = Seq(Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7)

    // Shifts word alignment by one bit.
    val ALIGNWD = in(Bool()) default(False)
    val ECLK = in(Bool())
    val SCLK = in(Bool())
    val RST = in(Bool())
  }
  noIoPrefix()
  mapCurrentClockDomain(io.SCLK, io.RST)
}


case class IDDRX5() extends BlackBox {
  val io = new Bundle {
    val D = in(Bool())

    // Positive edge
    val Q0 = out(Bool())
    val Q1 = out(Bool())
    val Q2 = out(Bool())
    val Q3 = out(Bool())
    val Q4 = out(Bool())
    val Q5 = out(Bool())
    val Q6 = out(Bool())
    val Q7 = out(Bool())
    val Q8 = out(Bool())
    val Q9 = out(Bool())

    val Q = Seq(Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9)

    // Shifts word alignment by one bit.
    val ALIGNWD = in(Bool()) default(False)
    val ECLK = in(Bool())
    val SCLK = in(Bool())
    val RST = in(Bool())
  }
  noIoPrefix()
  mapCurrentClockDomain(io.SCLK, io.RST)
}

case class IDDR71() extends BlackBox {
  val io = new Bundle {
    val D = in(Bool())

    // Positive edge
    val Q0 = out(Bool())
    val Q1 = out(Bool())
    val Q2 = out(Bool())
    val Q3 = out(Bool())
    val Q4 = out(Bool())
    val Q5 = out(Bool())
    val Q6 = out(Bool())

    val Q = Seq(Q0, Q1, Q2, Q3, Q4, Q5, Q6)

    // Shifts word alignment by one bit.
    val ALIGNWD = in(Bool()) default(False)
    val ECLK = in(Bool())
    val SCLK = in(Bool())
    val RST = in(Bool())
  }
  noIoPrefix()
  mapCurrentClockDomain(io.SCLK, io.RST)
}