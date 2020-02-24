// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import Chisel._

import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.InOrderArbiter

case object BuildRoCC extends Field[Seq[Parameters => LazyRoCC]](Nil)

class RoCCInstruction extends Bundle {
  val funct = Bits(width = 7)
  val rs2 = Bits(width = 5)
  val rs1 = Bits(width = 5)
  val xd = Bool() // set if destination register exsist
  val xs1 = Bool()// set if soruce resister exsist
  val xs2 = Bool()
  val rd = Bits(width = 5) // destination resister id
  val opcode = Bits(width = 7)
}

class RoCCCommand(implicit p: Parameters) extends CoreBundle()(p) {
  val inst = new RoCCInstruction
  val rs1 = Bits(width = xLen)
  val rs2 = Bits(width = xLen)
  val status = new MStatus
}

class RoCCResponse(implicit p: Parameters) extends CoreBundle()(p) {
  val rd = Bits(width = 5)
  val data = Bits(width = xLen)
}

class RoCCCoreIO(implicit p: Parameters) extends CoreBundle()(p) {
  val cmd = Decoupled(new RoCCCommand).flip
  val resp = Decoupled(new RoCCResponse)
  val mem = new HellaCacheIO
  val busy = Bool(OUTPUT)
  val interrupt = Bool(OUTPUT)
  val exception = Bool(INPUT)
}

class RoCCIO(val nPTWPorts: Int)(implicit p: Parameters) extends RoCCCoreIO()(p) {
  val ptw = Vec(nPTWPorts, new TLBPTWIO)
  val fpu_req = Decoupled(new FPInput)
  val fpu_resp = Decoupled(new FPResult).flip
}

/** Base classes for Diplomatic TL2 RoCC units **/
abstract class LazyRoCC(
      val opcodes: OpcodeSet,
      val nPTWPorts: Int = 0,
      val usesFPU: Boolean = false
    )(implicit p: Parameters) extends LazyModule {
  val module: LazyRoCCModuleImp
  val atlNode: TLNode = TLIdentityNode()
  val tlNode: TLNode = TLIdentityNode()
}

class LazyRoCCModuleImp(outer: LazyRoCC) extends LazyModuleImp(outer) {
  val io = IO(new RoCCIO(outer.nPTWPorts))
}

/** Mixins for including RoCC **/

trait HasLazyRoCC extends CanHavePTW { this: BaseTile =>
  val roccs = p(BuildRoCC).map(_(p))

  roccs.map(_.atlNode).foreach { atl => tlMasterXbar.node :=* atl }
  roccs.map(_.tlNode).foreach { tl => tlOtherMastersNode :=* tl }

  nPTWPorts += roccs.map(_.nPTWPorts).foldLeft(0)(_ + _)
  nDCachePorts += roccs.size
}

trait HasLazyRoCCModule extends CanHavePTWModule
    with HasCoreParameters { this: RocketTileModuleImp with HasFpuOpt =>

  val (respArb, cmdRouter) = if(outer.roccs.size > 0) {
    val respArb = Module(new RRArbiter(new RoCCResponse()(outer.p), outer.roccs.size))
    val cmdRouter = Module(new RoccCommandRouter(outer.roccs.map(_.opcodes))(outer.p))
    outer.roccs.zipWithIndex.foreach { case (rocc, i) =>
      ptwPorts ++= rocc.module.io.ptw
      rocc.module.io.cmd <> cmdRouter.io.out(i)
      val dcIF = Module(new SimpleHellaCacheIF()(outer.p))
      dcIF.io.requestor <> rocc.module.io.mem
      dcachePorts += dcIF.io.cache
      respArb.io.in(i) <> Queue(rocc.module.io.resp)
    }

    fpuOpt foreach { fpu =>
      val nFPUPorts = outer.roccs.filter(_.usesFPU).size
      if (usingFPU && nFPUPorts > 0) {
        val fpArb = Module(new InOrderArbiter(new FPInput()(outer.p), new FPResult()(outer.p), nFPUPorts))
        val fp_rocc_ios = outer.roccs.filter(_.usesFPU).map(_.module.io)
        fpArb.io.in_req <> fp_rocc_ios.map(_.fpu_req)
        fp_rocc_ios.zip(fpArb.io.in_resp).foreach {
          case (rocc, arb) => rocc.fpu_resp <> arb
        }
        fpu.io.cp_req <> fpArb.io.out_req
        fpArb.io.out_resp <> fpu.io.cp_resp
      } else {
        fpu.io.cp_req.valid := Bool(false)
        fpu.io.cp_resp.ready := Bool(false)
      }
    }
    (Some(respArb), Some(cmdRouter))
  } else {
    (None, None)
  }
}


    
    
    
    
    
     


    
class  AccumulatorExample(opcodes: OpcodeSet, val n: Int = 4)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new AccumulatorExampleModuleImp(this)
}


class AccumulatorExampleModuleImp(outer: AccumulatorExample)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {
  
  val regfile = Mem(outer.n, UInt(width = xLen)) //Mem(4,64)
  val busy = Reg(init = Vec.fill(outer.n){Bool(false)}) // val myVec1 = vec.fill(Number of element){data_type/size_width}
                                                        // esample MyVec(1) = 

  val cmd = Queue(io.cmd)
  val funct = cmd.bits.inst.funct
  val addr = cmd.bits.rs2(log2Up(outer.n)-1,0) //[3:0] Compute the log2 rounded up with min value of 1

  val doWrite = funct === UInt(0)
  val doRead = funct === UInt(1)
  val doLoad = funct === UInt(2)
  val doAccum = funct === UInt(3)
  val doBCDadd = funct ===UInt(4)
  //val doClear = funct === UInt(5)
  val doConvert = funct ===UInt(6)
  val doBCDmul = funct ===UInt(7)
  val doBCD = funct === UInt(8)
  val doDPD = funct ===UInt(9)
  val doReadTw = funct ===UInt(10)
  val memRespTag = io.mem.resp.bits.tag(log2Up(outer.n)-1,0)

  // datapath
  val addend = cmd.bits.rs1
  val addi = cmd.bits.rs2
  val accum = regfile(addr)
  //decimal===============================
  //val preCompute = Reg(Vec(10,Bits(68.W)))
  val preCompute = Reg(Vec(Seq.fill(10)(0.U(68.W))))
 // val pp = Reg(Vec(16,Bits(68.W)))
  val pp = Reg(Vec(Seq.fill(16)(0.U(68.W))))

  val multiplicand_X =  Reg(UInt(width = xLen)) 
  val multiplier_Y =  Reg(UInt(width = xLen)) 
  
  //conversion
 // val BCDConv = Reg(UInt(42.W))
  //val wdata = Mux(doWrite, addend, accum + addend)
   val wdata = Mux(doWrite, addend, accum + addend)
            
 

  //state machine 

  /*
when (cmd.fire() && doClear)
  {
       product_R := 0.U
       multiplier_Y := 0.U
       multiplicand_X := 0.U
  }*/

when(cmd.fire() && (doDPD))
{
  printf("BDC to DPD conversion start input 60 output 50 bit\n")

 def encoding_bcd (a_val:Bits):UInt  =
 {

  // module encoding(a,b,c,d,e,f,g,h,i,j,k,m,p,q,r,s,t,u,v,w,x,y);
  val in = Wire(Bits(10.W))//Wire(Vec(4,Bits(1.W)))
  // val p,q,r,s,t,u,v,w,x,y;
  val  p,q,r,s,t,u,v,w,x,y= Wire(Bits(1.W))
  val sum = Wire(Vec(12,Bool()))
     in :=a_val 
     /*
     in(0) := p
     in(1) := q
     in(2) := r
     in(3) := s
     in(4) := t
     in(5) := u
     in(6) := v
     in(7) := w
     in(8) := x
     in(9) := y
     */

     p := in(0)
     q := in(1)
     r := in(2)
     s := in(3)
     t := in(4)
     u := in(5) 
     v := in(6)
     w := in(7)
     x := in(8)
     y:= in(9)


     sum(0) := (~s & v & w) | (t & v & w & x) | (v & w & ~x)
     sum(1) := (p & s & x) | (p & ~w) | (p & ~v)
     sum(2) := (q & s & x) | (q & ~w) | (q & ~v)
     sum(3) := r
     sum(4) := (t & v & ~w & x) | (s & v & w & x) | (~t & v & x)
     sum(5) := (p & t & v & w & x) | (s & ~x) | (s & ~v)
     sum(6) := (q & t & w) | (t & ~x) | (t & ~v)
     sum(7) := u
     sum(8) := (t & v & w & x) | (s & v & w & x) | (v & ~w & ~x)
     sum(9) := (p & ~s & ~t & w) | (s & v & ~w & x) | (p & w & ~x) | (~v & w)
     sum(10) := ( q & ~s & ~t & v & w) | (q & v & w & ~x) | (t & v & ~w & x) | (~v & x)
     sum(11) := y

   return (sum.asUInt)
  }

  // This method takes two 64 bit number  and one one bit number as input and reuten one 64 bit with one carry as output;
  def sixtyfourbit (a_val:Bits) :Bits=
    {
    val in = Wire (Bits(50.W))
    val sum = Wire(Bits(60.W))
        in:=a_val
      // a := addend_1
      // b := addend_2
    val(sum1)  =    encoding_bcd (in(9,0))
    val(sum2)  =    encoding_bcd(in(19,10))
    val(sum3)  =    encoding_bcd(in(29,20))
    val(sum4)  =    encoding_bcd(in(39,30))
    val(sum5)  =    encoding_bcd(in(49,40))
      sum := Cat(sum1,sum2,sum3,sum4,sum5)
    return (sum.asUInt)
   }




  regfile(addr) :=   sixtyfourbit (cmd.bits.rs1)
  printf("BCD to DPD reg value = %x, result = %x \n", cmd.bits.rs1, regfile(addr))
  //printf("BCD to DPD option datapaht a = %x, b = %x, result = %x \n", addend,addi,accum)



}


when(cmd.fire() && (doBCD))
{
      printf("DPD to BCD conversion start input 50 output 60\n")
  def encoding_dpd (a_val:Bits):UInt  =
    {

      // module encoding(a,b,c,d,e,f,g,h,i,j,k,m,p,q,r,s,t,u,v,w,x,y);
      val in = Wire(Bits(12.W))//Wire(Vec(4,Bits(1.W)))
      // val p,q,r,s,t,u,v,w,x,y;
      val a,b,c,d,e,f,g,h,i,j,k,m = Wire(Bits(1.W))
      val s = Wire(Vec(10,Bool()))
     in := a_val

     a := in(0)
     b := in(1)
     c := in(2)
     d := in(3)
     e := in(4)
     f := in(5)
     g := in(6)
     h := in(7)
     i := in(8)
     j := in(9)
     k := in(10)
     m := in(11)



     s(0) := (a & f & i) | (a & j) | b 
     s(1) := (a & g & i) | (a & k) | c
     s(2) := d
     s(3) := (~a & e & j) | (f & ~i) | (~a & f) | (e & i)
     s(4) := (~a & e & k) | (a & i) | g
     s(5) := h
     s(6) := a | e | i
     s(7) := (~e & j) | (e & i) | a
     s(8) := (~a & k) | (a & i) | e
     s(9) := m
  
     return (s.asUInt)
  }

  // This method takes two 64 bit number  and one one bit number as input and reuten one 64 bit with one carry as output;
  def sixtyfourbit (a_val:Bits) :Bits=
  {
    val in = Wire (Bits(60.W))
    val sum = Wire(Bits(50.W))
        in := a_val
      // b := addend_2
    val(sum1)  =     encoding_dpd (in(11,0))
    val(sum2)  =     encoding_dpd (in(23,12))
    val(sum3)  =     encoding_dpd (in(35,24))
    val(sum4)  =     encoding_dpd (in(47,36))
    val(sum5)  =     encoding_dpd (in(59,48))
      sum := Cat(sum1,sum2,sum3,sum4,sum5)
    return (sum.asUInt)
  }




  regfile(addr) :=   sixtyfourbit (cmd.bits.rs1)
  printf("PDP to BCD reg value = %x, result = %x \n", cmd.bits.rs1, regfile(addr))
 // printf("DPD to BCD option datapaht a = %x, b = %x, result = %x \n", addend,addi,accum)




}


//==========================BCD add =================================

when(cmd.fire() && (doBCDadd))
{
    printf("BDC add start\n")
    printf("address rs1, rs2, rd %x, %x, %x \n",  cmd.bits.inst.rs1 ,  cmd.bits.inst.rs2, cmd.bits.inst.rd)
    printf(" state check doWrite, doRed, doLoad, doAccum, doBCD,Doconvert,domul %x, %x, %x, %x, %x, %x, %x \n",  doWrite, doRead,doLoad,doAccum, doBCDadd,doConvert,doBCDmul)
    printf("RS1(data), RS2(dat) %x,%x\n", cmd.bits.rs1, cmd.bits.rs2)
 

    // decimal one digit CLA it takes one BCD digit with 1 bit carry and returen 4 bit sum and one bit carry
      def CLA (a:Bits, b:Bits, carry:UInt):(Bits,UInt) =
      {
        val cin  = Wire (UInt(1.W))
        cin := carry
        val gdigit, pdigit,cout,k,l,c1 = Wire (Bits(1.W))
        val a1,b1,g,p,h = Wire(Bits(4.W))//Wire(Vec(4,Bits(1.W)))
        val s = Wire(Vec(4,Bool()))
          a1:=a
          b1:=b
          g:=a1&b1 
          p:=a1|b1
          h:=a1^b1

          k := ( p(3) | g(2) ) & ( p(3) | p(1) ) & ( g(3) | p(2) | p(1) )
          l := ( p(3) | p(2) ) & ( p(3) | g(2) | g(1) )
          cout := k | ( l & c1 )
          c1 := ( a(0) & b(0) ) | ( a(0) & cin ) | ( b(0) & cin )

       //equation for sum bit
          s(0) := h(0) ^ cin
          s(1) := ( h(1) & ~c1 & ~k ) | ( h(1) & c1 & l ) | ( ~h(1) & c1 & ~l ) | ( ~h(1) & ~c1 & k )
          s(2) := ( ~p(2) & g(1) ) | ( ~p(3) & h(2) & ~p(1) ) | ( ~p(3) & ~p(2) & p(1) & c1 ) | ( g(2) & g(1) & c1 ) | ( p(3) & p(2) & c1 ) |( g(3) & ~c1 ) | ( h(2) & h(1) & ~c1 )
          s(3) := ( g(3) & c1 ) | ( ~h(3) & h(2) & h(1) & c1 ) | ( l & ~k  & ~c1 )

      // a + b = 9, pdigit = 1; a + b >= 10, gdigit = 1;
          gdigit := k | ( l & g(0) );
          pdigit := l & h(0);
  
      return (s.asUInt,cout.asUInt)
      } 
 


    // This method takes two 64 bit number  and one one bit number as input and reuten one 64 bit with one carry as output;
    def seventeenDigitCLA (addend_1:Bits, addend_2:Bits) :Bits=
    {
      val a = Wire (Bits(68.W))
      val b = Wire (Bits(68.W))
      val sum = Wire(Bits(68.W))
        a := addend_1
        b := addend_2
      val(sum1,carry1)  =  CLA (a(3,0),    b(3,0),   0.U)
      val(sum2,carry2)  =  CLA (a(7,4),    b(7,4),   carry1)
      val(sum3,carry3)  =  CLA (a(11,8),   b(11,8),  carry2)
      val(sum4,carry4)  =  CLA (a(15,12),  b(15,12), carry3)
      val(sum5,carry5)  =  CLA (a(19,16),  b(19,16), carry4)
      val(sum6,carry6)  =  CLA (a(23,20),  b(23,20), carry5)
      val(sum7,carry7)  =  CLA (a(27,24),  b(27,24), carry6)
      val(sum8,carry8)  =  CLA (a(31,28),  b(31,28), carry7)
      val(sum9,carry9)  =  CLA (a(35,32),  b(35,32), carry8)
      val(sum10,carry10) =  CLA (a(39,36),  b(39,36), carry9)
      val(sum11,carry11) =  CLA (a(43,40),  b(43,40),carry10)
      val(sum12,carry12) =  CLA (a(47,44),  b(47,44),carry11)
      val(sum13,carry13) =  CLA (a(51,48),  b(51,48),carry12)
      val(sum14,carry14) =  CLA (a(55,52),  b(55,52),carry13)
      val(sum15,carry15) =  CLA (a(59,56),  b(59,56),carry14)
      val(sum16,carry16) =  CLA (a(63,60),  b(63,60),carry15)
      val(sum17,carry17) =  CLA (a(67,64),  b(67,64),carry16)
        sum := Cat(sum17,sum16,sum15,sum14,sum13,sum12,sum11,sum10,sum9,sum8,sum7,sum6,sum5,sum4,sum3,sum2,sum1)
      return (sum.asUInt)
    }
        regfile(addr) :=  seventeenDigitCLA (cmd.bits.rs1,cmd.bits.rs1)
        printf("BCD ADD opton reg a = %x, b = %x, result = %x \n", cmd.bits.rs1,cmd.bits.rs2, regfile(addr))
        printf("BCD ADD option datapaht a = %x, b = %x, result = %x \n", addend,addi,accum)
}

//===========================converter===================================
when(cmd.fire() && (doConvert))
{

      printf("conversion start with bug fix")
      printf("address rs1, rs2, rd %x, %x, %x \n",  cmd.bits.inst.rs1 ,  cmd.bits.inst.rs2, cmd.bits.inst.rd)
      printf(" state check doWrite, doRed, doLoad, doAccum, doBCD,Doconvert,domul %x, %x, %x, %x, %x, %x, %x \n",  doWrite, doRead,doLoad,doAccum, doBCDadd,doConvert,doBCDmul)
      printf("RS1(data), RS2(dat) %x,%x\n", cmd.bits.rs1, cmd.bits.rs2)
    val b0, b1, b2, b3, b4, b5, b6, b7, b8,b9, b10, b11, b12, b13, b14, b15, b16,b17, b18, b19, b20,b21, b22, b23, b24,b25 = Wire(Bits(4.W))
    val a0, a1, a2, a3, a4, a5, a6, a7, a8,a9, a10, a11, a12, a13, a14, a15,a16,a17, a18, a19, a20,a21, a22, a23, a24, a25,a26,  a27,a28 = Wire(Bits(4.W))
    val c0, c1, c2, c3, c4, c5, c6, c7, c8,c9, c10, c11, c12, c13, c14, c15,c16,c17, c18, c19, c20,c21, c22, c23 = Wire(Bits(4.W))
    val d0, d1, d2, d3, d4, d5, d6, d7, d8,d9, d10, d11, d12, d13, d14, d15,d16,d17, d18, d19 = Wire(Bits(4.W))
    val e0, e1, e2, e3, e4, e5, e6, e7, e8,e9, e10, e11, e12, e13, e14, e15,e16= Wire(Bits(4.W))
    val f0, f1, f2, f3, f4, f5, f6, f7, f8,f9, f10, f11, f12, f13= Wire(Bits(4.W))
    val g0, g1, g2, g3, g4, g5, g6, g7, g8,g9, g10 = Wire(Bits(4.W))
    val h0, h1, h2, h3, h4, h5, h6, h7= Wire(Bits(4.W))
    val i0, i1, i2, i3, i4,i5= Wire(Bits(4.W))
    val j0, j1= Wire(Bits(4.W))

    def add3(a:Bits):Bits=
    {
    val in,out = Wire (Bits(4.W))
       in :=a
       when     (in=== Bits("b0000")) { out := Bits("b0000")} 
      .elsewhen (in=== Bits("b0001")) { out := Bits("b0001")}
      .elsewhen (in=== Bits("b0010")) { out := Bits("b0010")}
      .elsewhen (in=== Bits("b0011")) { out := Bits("b0011")}
      .elsewhen (in=== Bits("b0100")) { out := Bits("b0100")}
      .elsewhen (in=== Bits("b0101")) { out := Bits("b1000")}
      .elsewhen (in=== Bits("b0110")) { out := Bits("b1001")}
      .elsewhen (in=== Bits("b0111")) { out := Bits("b1010")}
      .elsewhen (in=== Bits("b1000")) { out := Bits("b1011")}
      .elsewhen (in=== Bits("b1001")) { out := Bits("b1100")}
      .otherwise { out := Bits("b0000")}
      printf("in and out are %x, %x\n",in,out)
    return out.asUInt
    }//end add3


    //binary to decimal conversion shift-ad3 algorithm 
    def binary_BCD (a:Bits) : Bits = 
    {
      val in  = Wire (Bits(32.W))
      val out = Wire (Bits(42.W))
      val out1,out2,out3,out4,out5,out6,out7,out8,out9,out10 = Wire (Bits(4.W))
      val out11 = Wire(Bits(1.W))
      in :=a
      a0 :=  add3 (Cat(Bits("b0"),in(31,29)))
      a1 :=  add3 (Cat(a0(2,0),in(28)))
      a2 :=  add3 (Cat(a1(2,0),in(27)))
      a3 :=  add3 (Cat(a2(2,0),in(26)))
      a4 :=  add3 (Cat(a3(2,0),in(25)))
      a5 :=  add3 (Cat(a4(2,0),in(24)))
      a6 :=  add3 (Cat(a5(2,0),in(23)))
      a7 :=  add3 (Cat(a6(2,0),in(22)))
      a8 :=  add3 (Cat(a7(2,0),in(21)))
      a9 :=  add3 (Cat(a8(2,0),in(20)))
      a10 :=  add3 (Cat(a9(2,0),in(19)))
      a11 :=  add3 (Cat(a10(2,0),in(18)))
      a12 :=  add3 (Cat(a11(2,0),in(17)))
      a13 :=  add3 (Cat(a12(2,0),in(16)))
      a14 :=  add3 (Cat(a13(2,0),in(15)))
      a15 :=  add3 (Cat(a14(2,0),in(14)))
      a16 :=  add3 (Cat(a15(2,0),in(13)))
      a17 :=  add3 (Cat(a16(2,0),in(12)))
      a18 :=  add3 (Cat(a17(2,0),in(11)))
      a19 :=  add3 (Cat(a18(2,0),in(10)))
      a20 :=  add3 (Cat(a19(2,0),in(9)))
      a21 :=  add3 (Cat(a20(2,0),in(8)))
      a22 :=  add3 (Cat(a21(2,0),in(7)))
      a23 :=  add3 (Cat(a22(2,0),in(6)))
      a24 :=  add3 (Cat(a23(2,0),in(5)))
      a25 :=  add3 (Cat(a24(2,0),in(4)))
      a26 :=  add3 (Cat(a25(2,0),in(3)))
      a27 :=  add3 (Cat(a26(2,0),in(2)))
      out1 :=  add3 (Cat(a27(2,0),in(1))) // 4:1
      printf("friat bit finish 6 %x\n",out1)

      b0 :=  add3 (Cat(Bits("b0"),a0(3),a1(3),a2(3)))
      b1 :=  add3 (Cat(b0(2,0),a3(3)))
      b2 := add3 (Cat(b1(2,0),a4(3)))
      b3 := add3 (Cat(b2(2,0),a5(3)))
      b4 := add3 (Cat(b3(2,0),a6(3)))
      b5 := add3 (Cat(b4(2,0),a7(3)))
      b6 := add3 (Cat(b5(2,0),a8(3)))
      b7 := add3 (Cat(b6(2,0),a9(3)))
      b8 := add3 (Cat(b7(2,0),a10(3)))
      b9 := add3 (Cat(b8(2,0),a11(3)))
      b10 := add3 (Cat(b9(2,0),a12(3)))
      b11 := add3 (Cat(b10(2,0),a13(3)))
      b12 := add3 (Cat(b11(2,0),a14(3)))
      b13 := add3 (Cat(b12(2,0),a15(3)))
      b14 := add3 (Cat(b13(2,0),a16(3)))
      b15 := add3 (Cat(b14(2,0),a17(3)))
      b16 := add3 (Cat(b15(2,0),a18(3)))
      b17 := add3 (Cat(b16(2,0),a19(3)))
      b18 := add3 (Cat(b17(2,0),a20(3)))
      b19 := add3 (Cat(b18(2,0),a21(3)))
      b20 := add3 (Cat(b19(2,0),a22(3)))
      b21 := add3 (Cat(b20(2,0),a23(3)))
      b22 := add3 (Cat(b21(2,0),a24(3)))
      b23 := add3 (Cat(b22(2,0),a25(3)))
      b24 := add3 (Cat(b23(2,0),a26(3)))

      out2 := add3 (Cat(b24(2,0),a27(3))) //8:5
      printf("second bit should 1 %x\n",out2)

      c0  :=  add3 (Cat(Bits("b0"),b0(3),b1(3),b2(3)))
      c1  :=  add3 (Cat(c0(2,0),b3(3)))
      c2  := add3 (Cat(c1(2,0),b4(3)))
      c3  := add3 (Cat(c2(2,0),b5(3)))
      c4  := add3 (Cat(c3(2,0),b6(3)))
      c5  := add3 (Cat(c4(2,0),b7(3)))
      c6  := add3 (Cat(c5(2,0),b8(3)))
      c7  := add3 (Cat(c6(2,0),b9(3)))
      c8  := add3 (Cat(c7(2,0),b10(3)))
      c9  := add3 (Cat(c8(2,0),b11(3)))
      c10 := add3 (Cat(c9(2,0),b12(3)))
      c11 := add3 (Cat(c10(2,0),b13(3)))
      c12 := add3 (Cat(c11(2,0),b14(3)))
      c13 := add3 (Cat(c12(2,0),b15(3)))
      c14 := add3 (Cat(c13(2,0),b16(3)))
      c15 := add3 (Cat(c14(2,0),b17(3)))
      c16 := add3 (Cat(c15(2,0),b18(3)))
      c17 := add3 (Cat(c16(2,0),b19(3)))
      c18 := add3 (Cat(c17(2,0),b20(3)))
      c19 := add3 (Cat(c18(2,0),b21(3)))
      c20 := add3 (Cat(c19(2,0),b22(3)))
      c21 := add3 (Cat(c20(2,0),b23(3)))

      out3 := add3 (Cat(c21(2,0),b24(3))) // 12-9
      printf("Third bit should 2 %x\n",out3)

      d0  :=  add3 (Cat(Bits("b0"),c0(3),c1(3),c2(3)))
      d1  :=  add3 (Cat(d0(2,0),c3(3)))
      d2  := add3 (Cat(d1(2,0),c4(3)))
      d3  := add3 (Cat(d2(2,0),c5(3)))
      d4  := add3 (Cat(d3(2,0),c6(3)))
      d5  := add3 (Cat(d4(2,0),c7(3)))
      d6  := add3 (Cat(d5(2,0),c8(3)))
      d7  := add3 (Cat(d6(2,0),c9(3)))
      d8  := add3 (Cat(d7(2,0),c10(3)))
      d9  := add3 (Cat(d8(2,0),c11(3)))
      d10 := add3 (Cat(d9(2,0),c12(3)))
      d11 := add3 (Cat(d10(2,0),c13(3)))
      d12 := add3 (Cat(d11(2,0),c14(3)))
      d13 := add3 (Cat(d12(2,0),c15(3)))
      d14 := add3 (Cat(d13(2,0),c16(3)))
      d15 := add3 (Cat(d14(2,0),c17(3)))
      d16 := add3 (Cat(d15(2,0),c18(3)))
      d17 := add3 (Cat(d16(2,0),c19(3)))
      d18 := add3 (Cat(d17(2,0),c20(3)))


      out4 := add3 (Cat(d18(2,0),c21(3)))//13:16

      e0  :=  add3 (Cat(Bits("b0"),d0(3),d1(3),d2(3)))
      e1  :=  add3 (Cat(e0(2,0),d3(3)))
      e2  := add3 (Cat(e1(2,0),d4(3)))
      e3  := add3 (Cat(e2(2,0),d5(3)))
      e4  := add3 (Cat(e3(2,0),d6(3)))
      e5  := add3 (Cat(e4(2,0),d7(3)))
      e6  := add3 (Cat(e5(2,0),d8(3)))
      e7  := add3 (Cat(e6(2,0),d9(3)))
      e8  := add3 (Cat(e7(2,0),d10(3)))
      e9  := add3 (Cat(e8(2,0),d11(3)))
      e10 := add3 (Cat(e9(2,0),d12(3)))
      e11 := add3 (Cat(e10(2,0),d13(3)))
      e12 := add3 (Cat(e11(2,0),d14(3)))
      e13 := add3 (Cat(e12(2,0),d15(3)))
      e14 := add3 (Cat(e13(2,0),d16(3)))
      e15 := add3 (Cat(e14(2,0),d17(3)))

      out5 := add3 (Cat(e15(2,0),d18(3))) //17:20


      f0 :=  add3 (Cat(Bits("b0"),e0(3),e1(3),e2(3)))
      f1 :=  add3 (Cat(f0(2,0),e3(3)))
      f2 :=  add3 (Cat(f1(2,0),e4(3)))
      f3 :=  add3 (Cat(f2(2,0),e5(3)))
      f4 :=  add3 (Cat(f3(2,0),e6(3)))
      f5 :=  add3 (Cat(f4(2,0),e7(3)))
      f6 :=   add3 (Cat(f5(2,0),e8(3)))
      f7 :=  add3 (Cat(f6(2,0),e9(3)))
      f8 :=  add3 (Cat(f7(2,0),e10(3)))
      f9 :=  add3 (Cat(f8(2,0),e11(3)))
      f10:=  add3 (Cat(f9(2,0),e12(3)))
      f11:=  add3 (Cat(f10(2,0),e13(3)))
      f12:=  add3 (Cat(f11(2,0),e14(3)))

      out6 := add3 (Cat(f12(2,0),e15(3))) // 21-24

      g0 := add3 (Cat(Bits("b0"),f0(3),f1(3),f2(3)))
      g1 := add3 (Cat(g0(2,0),f3(3)))
      g2 := add3 (Cat(g1(2,0),f4(3)))
      g3 := add3 (Cat(g2(2,0),f5(3)))
      g4 := add3 (Cat(g3(2,0),f6(3)))
      g5 := add3 (Cat(g4(2,0),f7(3)))
      g6 := add3 (Cat(g5(2,0),f8(3)))
      g7 := add3 (Cat(g6(2,0),f9(3)))
      g8 := add3 (Cat(g7(2,0),f10(3)))
      g9 := add3 (Cat(g8(2,0),f11(3)))

      out7 := add3 (Cat(g9(2,0),f12(3))) // 25:28

      h0 := add3 (Cat(Bits("b0"),g0(3),g1(3),g2(3)))
      h1 := add3 (Cat(h0(2,0),g3(3)))
      h2 := add3 (Cat(h1(2,0),g4(3)))
      h3 := add3 (Cat(h2(2,0),g5(3)))
      h4 := add3 (Cat(h3(2,0),g6(3)))
      h5 := add3 (Cat(h4(2,0),g7(3)))
      h6 := add3 (Cat(h5(2,0),g8(3)))
      
      out8 := add3 (Cat(h6(2,0),g9(3))) // 32:29


      i0 := add3 (Cat(Bits("b0"),h0(3),h1(3),h2(3)))
      i1 := add3 (Cat(i0(2,0),h3(3)))
      i2 := add3 (Cat(i1(2,0),h4(3)))
      i3 := add3 (Cat(i2(2,0),h5(3)))
      //i4 := add3 (Cat(i3(2,0),h6(3)))

      out9 := add3 (Cat(i3(2,0),h6(3))) // 33:36

      j0 :=  add3 (Cat(Bits("b0"),i0(3),i1(3),i2(3)))

      out10 := add3 (Cat(j0(2,0),i3(3))) //37-40

      out11 := j0(3)

      out := Cat(out11,out10,out9,out8,out7,out6,out5,out4,out3,out2,out1,in(0))

      //regfile(addr) := out 



      printf("conversion result of new %x is %x\n",in,out)

    return  out.asUInt

    } //end binary_BCD 

    //val BCD_val_test = Bits("b0111_0001_0001_0000_0110_0110_0111_1000")
    //val BCD_val_test = Bits("b0111_1001_1001_1000_0110_0111_0111_1001")
    //BCDConv := binary_BCD(cmd.bits.rs1)
    //regfile(addr) := wdata 
      regfile(addr) :=  binary_BCD(cmd.bits.rs1)

}//end do convert


// ============================================================================== 
  //variable declaration for BCD
  //added for only function 4 fro decimal operation
when(cmd.fire() && (doBCDmul))
{
  
      multiplicand_X :=cmd.bits.rs1
      multiplier_Y := cmd.bits.rs2
      // val product_R = RegInit(0.U)
      //  val cin= Reg(UInt(width=1))
      //print debuging perpuse
      printf("address rs1, rs2, rd %x, %x, %x \n",   multiplicand_X  ,  cmd.bits.inst.rs2, cmd.bits.inst.rd)
      printf(" state check doWrite, doRed, doLoad, doAccum, doBCD,Doconvert,domul %x, %x, %x, %x, %x, %x, %x \n",  doWrite, doRead,doLoad,doAccum, doBCDadd,doConvert,doBCDmul)
      printf("Xd, AddrsRs1, Address RS2, RS1(data), RS2(data) %x , %x, %x,%x,%x\n", cmd.bits.inst.xd, cmd.bits.inst.xs1,cmd.bits.inst.xd, addi,addend)
   
   // decimal one digit CLA it takes one BCD digit with 1 bit carry and returen 4 bit sum and one bit carry
    def CLA (a:Bits, b:Bits, carry:UInt):(Bits,UInt) =
      {
        val cin  = Wire (UInt(1.W))
          cin := carry
        val gdigit, pdigit,cout,k,l,c1 = Wire (Bits(1.W))
        val a1,b1,g,p,h = Wire(Bits(4.W))//Wire(Vec(4,Bits(1.W)))
        val s = Wire(Vec(4,Bool()))
          a1:=a
          b1:=b
          g:=a1&b1
          p:=a1|b1
          h:=a1^b1
          k := ( p(3) | g(2) ) & ( p(3) | p(1) ) & ( g(3) | p(2) | p(1) )
          l := ( p(3) | p(2) ) & ( p(3) | g(2) | g(1) )
          cout := k | ( l & c1 )
          c1 := ( a(0) & b(0) ) | ( a(0) & cin ) | ( b(0) & cin )
          //  printf("---CLA START---\n")
          //  printf("cin %x\n",cin)
           s(0) := h(0) ^ cin
          //printf("s0 %x\n",s(0).asUInt)

          s(1) := ( h(1) & ~c1 & ~k ) | ( h(1) & c1 & l ) | ( ~h(1) & c1 & ~l ) | ( ~h(1) & ~c1 & k )
          // printf("s1 %x\n",s(1).asUInt)

          s(2) := ( ~p(2) & g(1) ) | ( ~p(3) & h(2) & ~p(1) ) | ( ~p(3) & ~p(2) & p(1) & c1 ) | ( g(2) & g(1) & c1 ) | ( p(3) & p(2) & c1 ) |( g(3) & ~c1 ) | ( h(2) & h(1) & ~c1 )
          // printf("s2 %x\n",s(2).asUInt)
          s(3) := ( g(3) & c1 ) | ( ~h(3) & h(2) & h(1) & c1 ) | ( l & ~k  & ~c1 )
          //  printf("s3 %x\n",s(3).asUInt)

          // a + b = 9, pdigit = 1; a + b >= 10, gdigit = 1;
          gdigit := k | ( l & g(0) );
          pdigit := l & h(0);

        // printf("CLA RESULT a b cin sum carry ,%x,%x,%x,%x, %x\n",a,b, cin,s.asUInt , cout.asUInt)
        // printf("-----CLA END------")
      return (s.asUInt,cout.asUInt)
    } 
    /*
    // decimal one digit CLA it takes one BCD digit with 1 bit carry and returen 4 bit sum and one bit carry
    def CLA_ (a:Bits, b:Bits, carry:UInt):(Bits,UInt) =
    {

        val cin  = Wire (UInt(1.W))
        cin := carry
        val gdigit, pdigit,cout = Wire (Bits(1.W))
        val a1,b1,g,p,h = Wire(Bits(4.W))//Wire(Vec(4,Bits(1.W)))
        val s = Wire(Vec(4,Bool()))
          a1:=a
          b1:=b
          g (0) := a1 (0) & b1 (0) 
          g (1) := a1 (1) & b1 (1)
          g (2) := a1 (2) & b1 (2)
          g (3) := a1 (3) & b1 (3)

          p (0) := a1 (0) | b1 (0)
          p (1) := a1 (1) | b1 (1)
          p (2) := a1 (2) | b1 (2)
          p (3) := a1 (3) | b1 (3)

          h (0) := a1 (0) ^ b1 (0)
          h (1) := a1 (1) ^ b1 (1)
          h (2) := a1 (2) ^ b1 (2)
          h (3) := a1 (3) ^ b1 (3)

          gdigit := g(3) | ( p(3) & g(2) ) | ( p(3) & p(2) & g(1) ) | ( p(3) & p(2) & p(1) & g(0) )
          pdigit := h(3) & h(2) & h(1) & h(0)
        val c = "b0000".U(4.W)
          cout := gdigit | ( pdigit & cin ) 
          printf("---CLA START---\n")
          printf("cin %x\n",cin)
          s (0) := h (0) ^ c(0) 
          printf("s0 %x\n",s(0).asUInt)

   


          s(1) := ~cout &( p(3)&( p(2)&p(1) | p(2)&g(0) | p(2)&p(0)&cin | p(1)&g(0) | p(1)&p(0)&cin ) | g(2)&g(1) | p(2)&g(1)&p(0)&cin | ~s(3)&( p(2) | p(1) | g(0) | p(0)&cin ) )| g(3)&g(1)&( g(0) | p(0)&cin ) | cout& ~s(3)& ~( cin&pdigit | h(3)&h(2)&h(1)&g(0) | h(3)&h(2)&g(1)& ~p(0) | h(3)&g(2)&h(1)& ~p(0) | g(3)& ~p(2) | ~cin & h(3) & h(2) & g(1) & h(0) | ~cin & h(3)&g(2)&h(1) | ~cin&g(3)&h(2)& ~p(1)&h(0) )
  printf("s1 %x\n",s(1).asUInt)

          s(2) := ~cout&(p(3)&p(2) | p(3)&p(1) | p(3)&g(0) | p(3)&p(0)&cin | p(2)& ~p(0)& ~cin | p(2)&g(1) | p(2)&p(0)&cin | g(1)&(g(0) | p(0)&cin | ~p(0)& ~cin) | p(1)&(g(0)& ~cin | h(0)&cin)) | cout&( ~cin&g(3)&(p(1)&g(0) | p(1)& ~p(0) | g(2)&g(0)) | cin&(g(2)&g(1)&h(0) | g(3)&p(1)&h(0) | g(3)&g(2)&h(0)) | g(3)&g(1) | g(3)&g(2)&p(1)&g(0) | g(3)&g(2)&p(1)&p(0)&cin )
       
      printf("s2 %x\n",s(2).asUInt)

          s(3) := ~cout&(p(2) | p(2)&g(1) | p(2)&p(0) | p(2)&cin | g(1)&p(0) | g(1)&cin | p(1)&g(0)&cin) | g(3)&(g(1) | p(1)&g(2)&(p(0)|cin) | g(2)&g(0)&cin | p(2)&p(1)&g(0)&cin)
          printf("s3 %x\n",s(3).asUInt)
          printf("CLA RESULT a b cin sum carry ,%x,%x,%x,%x, %x\n",a,b, cin,s.asUInt , cout.asUInt)
          printf("-----CLA END------")
      return (s.asUInt,cout.asUInt)

    }
    */

    // This method takes two 64 bit number  and one one bit number as input and reuten one 64 bit with one carry as output;
    def seventeenDigitCLA (addend_1:Bits, addend_2:Bits) :Bits=
      {
        // val initialCarry = UInt(1.W)
        // initialCarry := 0.U
        val a = Wire (Bits(68.W))
        val b = Wire (Bits(68.W))
        // val sum1,sum2,sum3,sum4,sum5,sum6,sum7,sum8,sum9,sum10,sum11,sum12,sum13,sum14,sum15,sum16,sum17 = Wire(Bits(4.W))
        // val carry1,carry2,carry3,carry4,carry5,carry6,carry7,carry8,carry9,carry10,carry11,carry12,carry13,carry14,carry15,carry16,carry17 = Wire(Bits(1.W))
        val sum = Wire(Bits(68.W))
          a := addend_1
          b := addend_2  
        val(sum1,carry1)  =  CLA (a(3,0),    b(3,0),   0.U)
        val(sum2,carry2)  =  CLA (a(7,4),    b(7,4),   carry1)
        val(sum3,carry3)  =  CLA (a(11,8),   b(11,8),  carry2)
        val(sum4,carry4)  =  CLA (a(15,12),  b(15,12), carry3)
        val(sum5,carry5)  =  CLA (a(19,16),  b(19,16), carry4)
        val(sum6,carry6)  =  CLA (a(23,20),  b(23,20), carry5)
        val(sum7,carry7)  =  CLA (a(27,24),  b(27,24), carry6)
        val(sum8,carry8)  =  CLA (a(31,28),  b(31,28), carry7)
        val(sum9,carry9)  =  CLA (a(35,32),  b(35,32), carry8)
        val(sum10,carry10) =  CLA (a(39,36),  b(39,36), carry9)
        val(sum11,carry11) =  CLA (a(43,40),  b(43,40),carry10)
        val(sum12,carry12) =  CLA (a(47,44),  b(47,44),carry11)
        val(sum13,carry13) =  CLA (a(51,48),  b(51,48),carry12)
        val(sum14,carry14) =  CLA (a(55,52),  b(55,52),carry13)
        val(sum15,carry15) =  CLA (a(59,56),  b(59,56),carry14)
        val(sum16,carry16) =  CLA (a(63,60),  b(63,60),carry15)
        val(sum17,carry17) =  CLA (a(67,64),  b(67,64),carry16)
          // sum := Cat(sum1,sum2 ,sum3,sum4,sum5,sum6,sum7,sum8,sum9,sum10,sum11,sum12,sum13,sum14,sum15,sum16,sum17)
          sum := Cat(sum17,sum16,sum15,sum14,sum13,sum12,sum11,sum10,sum9,sum8,sum7,sum6,sum5,sum4,sum3,sum2,sum1)
          printf(" sum 1-17 are %x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x\n",sum1,sum2 ,sum3,sum4,sum5,sum6,sum7,sum8,sum9,sum10,sum11,sum12,sum13,sum14,sum15,sum16,sum17)
          printf("Seventeen Degit Adder OUTPUT %x, %x, %x \n", a,b,sum)
      return (sum.asUInt)
      //sum16._1.asUInt.asBool
    }
      //val pre1 =  seventeenDigitCLA(multiplicand_X, multiplicand_X)
      // printf("result of addition is %x",pre1)
      // This method takes two 71 bit number  and one one bit number as input and reuten one 71 bit(carry free ) with one carry as output;
    def eighteenDigitCLA (addend_1:Bits, addend_2:Bits) :Bits=
      {
        val a = Wire (Bits(72.W))
        val b = Wire (Bits(72.W))
          a := addend_1
          b:= addend_2
        val sum1 =  seventeenDigitCLA( a(67,0),b(67,0))
        val sum2 = CLA (a(71,68),    b(71,68), a(67))
        //val sum = Cat(sum1,sum2._1)
        val sum = Cat(sum2._1,sum1)
        return (sum.asUInt)
      } 

    def twentyDigitCLA (addend_1:Bits, addend_2:Bits) :Bits=
      {
        val sum = Wire (Bits(80.W))
        val a = Wire (Bits(80.W))
        val b = Wire (Bits(80.W))
          a := addend_1
          b:= addend_2
          printf("incomming of twenty digit is %x, %x",a,b)
          //val sum1 = seventeenDigitCLA( a(67,0),b(67,0))
      val(sum1,carry1)   =  CLA (a(3,0),    b(3,0),   0.U)
      val(sum2,carry2)   =  CLA (a(7,4),    b(7,4),   carry1)
      val(sum3,carry3)   =  CLA (a(11,8),   b(11,8),  carry2)
      val(sum4,carry4)   =  CLA (a(15,12),  b(15,12), carry3)
      val(sum5,carry5)   =  CLA (a(19,16),  b(19,16), carry4)
      val(sum6,carry6)   =  CLA (a(23,20),  b(23,20), carry5)
      val(sum7,carry7)   =  CLA (a(27,24),  b(27,24), carry6)
      val(sum8,carry8)   =  CLA (a(31,28),  b(31,28), carry7)
      val(sum9,carry9)   =  CLA (a(35,32),  b(35,32), carry8)
      val(sum10,carry10) =  CLA (a(39,36),  b(39,36), carry9)
      val(sum11,carry11) =  CLA (a(43,40),  b(43,40),carry10)
      val(sum12,carry12) =  CLA (a(47,44),  b(47,44),carry11)
      val(sum13,carry13) =  CLA (a(51,48),  b(51,48),carry12)
      val(sum14,carry14) =  CLA (a(55,52),  b(55,52),carry13)
      val(sum15,carry15) =  CLA (a(59,56),  b(59,56),carry14)
      val(sum16,carry16) =  CLA (a(63,60),  b(63,60),carry15)
      val(sum17,carry17) =  CLA (a(67,64),  b(67,64),carry16)
      val(sum18,carry18) =  CLA (a(71,68),  b(71,68),carry17)
      val(sum19,carry19) = CLA (a(75,72),   b(75,72),carry18)
      val(sum20,carry20) = CLA (a(79,76),   b(79,76),carry19)
        sum := Cat(sum20,sum19,sum18,sum17,sum16,sum15,sum14,sum13,sum12,sum11,sum10,sum9,sum8,sum7,sum6,sum5,sum4,sum3,sum2,sum1)
        printf(" sum 1-20 are %x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x\n",sum1,sum2 ,sum3,sum4,sum5,sum6,sum7,sum8,sum9,sum10,sum11,sum12,sum13,sum14,sum15,sum16,sum17,sum18,sum19,sum20)
        // val sum = Cat(sum1,sum2._1,sum3._1,sum4._1)
        //al sum = Cat(sum4._1,sum3._1,sum2._1,sum1)
      return (sum.asUInt)
    }

    def twentyfourDigitCLA (addend_1:Bits, addend_2:Bits) :Bits=
      {
        val a = Wire (Bits(96.W))
        val b = Wire (Bits(96.W))
          a := addend_1
          b:= addend_2
        val sum1 =  twentyDigitCLA( a(79,0),b(79,0))
        val sum2 = CLA (a(83,80),    b(83,80), a(79))
        val sum3 = CLA (a(87,84),    b(87,84), a(83))
        val sum4 = CLA (a(91,88),    b(91,88), a(87))
        val sum5 = CLA (a(95,92),    b(95,92), a(91))
        // val sum = Cat(sum1,sum2._1,sum3._1,sum4._1,sum5._1)
        val sum = Cat(sum5._1,sum4._1,sum3._1,sum2._1,sum1)
        return (sum.asUInt)
      }


      // preCoumpute is the value of 1X to 9X This porcess use CLA siquentially
      // and generate . 
      // val preCompute = Reg(Vec(10,Bits(68.W)))
      // val pre1 = Wire(Bits(68.W))
      preCompute(0) := Bits("b0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000")
      preCompute(1) := Cat(Bits("b0000"),multiplicand_X)
      preCompute(2) :=seventeenDigitCLA(preCompute(1), Cat(Bits("b0000"),multiplicand_X))
      preCompute(3) :=seventeenDigitCLA(preCompute(2), Cat(Bits("b0000"),multiplicand_X))
      preCompute(4) :=seventeenDigitCLA(preCompute(3), Cat(Bits("b0000"),multiplicand_X))
      preCompute(5) :=seventeenDigitCLA(preCompute(4), Cat(Bits("b0000"),multiplicand_X))
      preCompute(6) :=seventeenDigitCLA(preCompute(5), Cat(Bits("b0000"),multiplicand_X))
      preCompute(7) :=seventeenDigitCLA(preCompute(6), Cat(Bits("b0000"),multiplicand_X))
      preCompute(8) :=seventeenDigitCLA(preCompute(7), Cat(Bits("b0000"),multiplicand_X))
      preCompute(9) :=seventeenDigitCLA(preCompute(8), Cat(Bits("b0000"),multiplicand_X))
      printf("precom %x,%x,%x,%x,%x,%x,%x,%x,%x,%x\n", preCompute(0), preCompute(1), preCompute(2), preCompute(3), preCompute(4), preCompute(5), preCompute(6) ,preCompute(7), preCompute(8),preCompute(9) )

      /*
      for (i<-0 until 8) {
      //apreCompute(i+1) := preCompute(i) + multiplicand
      pre1 =  seventeenDigitCLA(preCompute(i), Cat(Bits("b0000"),multiplicand_X))
      preCompute(i+1) := pre1
      printf("value of 1x to 9x is %x \n", preCompute(i).asUInt)
       }
      */
      // val fourzero = Reg(Bits(width = 4))   
      // val eightzero= Reg(Bits(width=8)) 
      // val sixteenzero = Reg(Bits(width=16))    
      //  val thirtytwozero =Reg(Bits(width=32))
      // fourzero :=Bits("b0000")
      //  eightzero :=  Bits("b0000_0000")
      //  sixteenzero := Bits("b0000_0000_0000_0000")
      //  thirtytwozero := Bits("b0000_0000_0000_0000_0000_0000_0000_0000")
      /*

    printf("cat test")
    printf("4-8 %x\n",Cat(fourzero,eightzero))
    printf("8-4 %x\n",Cat(eightzero,fourzero))
    printf("8-4-4 %x\n",Cat(eightzero,fourzero,fourzero))


    val aH = Reg(Bits(width = 8))
    val aL = Reg(Bits(width=8))
    val A =  Reg(Bits(width=24))
    val A1 = Reg(Bits(width=24))
    aH := Bits("b0000_1000")//0x8//b0000_1000
    aL:= Bits("b1000_0001")//0x81//b1000_0001
    A := Cat(aH ,(Cat(aL,aH))) // 0000_1000_1000_0001_0000_1000
    printf("aH-aL %x\n",A)
    A1 := Cat(aH(7,4),aL(3,0))
    printf("after A1 %x\n",A1)
    */
    // here generate partial product based on digit of multi9plier    
    //   val pp = Reg(Vec(16,Bits(68.W)))

      pp(0) := preCompute(multiplier_Y(3,0))
      pp(1) := preCompute(multiplier_Y(7,4))
      pp(2) := preCompute(multiplier_Y(11,8))
      pp(3) := preCompute(multiplier_Y(15,12))
      pp(4) := preCompute(multiplier_Y(19,16))
      pp(5) := preCompute(multiplier_Y(23,20))
      pp(6) := preCompute(multiplier_Y(27,24))
      pp(7) := preCompute(multiplier_Y(31,28))
      pp(8) := preCompute(multiplier_Y(35,32))
      pp(9) := preCompute(multiplier_Y(39,36))
      pp(10) := preCompute(multiplier_Y(43,40))
      pp(11) := preCompute(multiplier_Y(47,44))
      pp(12) := preCompute(multiplier_Y(51,48))
      pp(13) := preCompute(multiplier_Y(55,52))
      pp(14) := preCompute(multiplier_Y(59,56))
      pp(15) := preCompute(multiplier_Y(63,60))
      printf("pp0-pp14 %x,\n %x\n, %x\n, %x\n, %x\n, %x\n, %x\n, %x\n, %x\n, %x\n, %x\n, %x\n, %x\n, %x\n,%x\n,%x\n",pp(0),pp(1),pp(2),pp(3),pp(4),pp(5),pp(6),pp(7),pp(8),pp(9),pp(10),pp(11),pp(12),pp(13),pp(14),pp(15))

    val level1_1, level1_2, level1_3, level1_4, level1_5, level1_6, level1_7, level1_8 = Wire(Bits(68.W))
    val level2_1, level2_2, level2_3, level2_4                                         = Wire(Bits(72.W))

    val level3_1, level3_2                                                             = Wire(Bits(80.W))
    val level4_1                                                                       = Wire(Bits(96.W))

      level1_1 := seventeenDigitCLA (Cat(Bits("b0000"), pp(0)(67,4).asUInt), pp(1))
    val level1_1_1 = Cat(level1_1,pp(0)(3,0))
      printf("level1_1_1 %x",level1_1_1) 

      level1_2 := seventeenDigitCLA (Cat(Bits("b0000"), pp(2)(67,4).asUInt), pp(3))
    val level1_1_2 = Cat(level1_2,pp(2)(3,0))
      printf("level1_1_2 %x",level1_1_2) 

      level1_3 := seventeenDigitCLA (Cat(Bits("b0000"), pp(4)(67,4).asUInt), pp(5))
    val level1_1_3 = Cat(level1_3,pp(4)(3,0))
      printf("level1_1_3 %x",level1_1_3) 

      level1_4 := seventeenDigitCLA (Cat(Bits("b0000"), pp(6)(67,4).asUInt), pp(7))
    val level1_1_4 = Cat(level1_4,pp(6)(3,0))
      printf("level1_1_4 %x",level1_1_4) 

      level1_5 := seventeenDigitCLA (Cat(Bits("b0000"), pp(8)(67,4).asUInt), pp(9))
    val level1_1_5 = Cat(level1_5,pp(8)(3,0))
      printf("level1_1_5 %x",level1_1_5) 

      level1_6 := seventeenDigitCLA (Cat(Bits("b0000"), pp(10)(67,4).asUInt), pp(11))
    val level1_1_6 = Cat(level1_6,pp(10)(3,0))
      printf("level1_1_6 %x",level1_1_6) 

      level1_7 := seventeenDigitCLA (Cat(Bits("b0000"), pp(12)(67,4).asUInt), pp(13))
    val level1_1_7 = Cat(level1_7,pp(12)(3,0))
      printf("level1_1_7 %x",level1_1_7) 

      level1_8 := seventeenDigitCLA (Cat(Bits("b0000"), pp(14)(67,4).asUInt), pp(15))
    val level1_1_8 = Cat(level1_8,pp(14)(3,0))
      printf("level1_1_8 %x",level1_1_8) 

      //18-digit accumulation
      level2_1 := eighteenDigitCLA (Cat(Bits("b00000000"), level1_1_1(71,8)), level1_1_2)
    val level2_1_1 = Cat(level2_1,level1_1_1(7,0))
      printf("level2_1_1 %x",level2_1_1) 
      level2_2 := eighteenDigitCLA (Cat(Bits("b00000000"), level1_1_3(71,8)), level1_1_4)
    val level2_1_2 = Cat(level2_2,level1_1_3(7,0))
      printf("level2_1_2 %x",level2_1_2) 

      level2_3 := eighteenDigitCLA (Cat(Bits("b00000000"), level1_1_5(71,8)), level1_1_6)
    val level2_1_3 = Cat(level2_3,level1_1_5(7,0))
      printf("level2_1_3 %x",level2_1_3) 

      level2_4 := eighteenDigitCLA (Cat(Bits("b00000000"), level1_1_7(71,8)), level1_1_8)
    val level2_1_4 = Cat(level2_4,level1_1_7(7,0))
      printf("level2_1_4 %x",level2_1_4) 
      // 20 digit accumulation 

      level3_1 := twentyDigitCLA (Cat(Bits("b0000000000000000"), level2_1_1(79,16)), level2_1_2)
    val level3_1_1 = Cat(level3_1,level2_1_1(15,0))
      printf("level3_1_1 %x",level3_1_1) 

      level3_2 := twentyDigitCLA (Cat(Bits("b0000000000000000"), level2_1_3(79,16)), level2_1_4)
    val level3_1_2 = Cat(level3_2,level2_1_3(15,0))
      printf("level3_1_2 %x",level3_1_2) 
      // 24 digit final 

      level4_1 := twentyfourDigitCLA (Cat( Bits("b0000_0000_0000_0000_0000_0000_0000_0000"), level3_1_1(95,32)), level3_1_2)
      //val product = Cat(level4_1,level3_1_1(31,0))
    val finalProduct = Cat(level4_1,level3_1_1(31,0))
      printf("coefficeent multilicatio result is %x",finalProduct)
      //printf("BCD prodcut is %x \n", product)
    
        regfile(addr) :=  finalProduct
        //Method-2 Parallel component this mehotd recieve elevelt emement and return final product as BCD
        //1//val BCD_Procuct = Wire(Bits(127.W))

        printf("end BCD \n") 

}//end doBCDmul



  //===================================================================================
when (cmd.fire() && (doWrite || doAccum)) 
{
     printf("Accumulator: Write  ")

  
  
  // decimal one digit CLA it takes one BCD digit with 1 bit carry and returen 4 bit sum and one bit carry
    def CLA (a:Bits, b:Bits, carry:UInt):(Bits,UInt) =
      {
        val cin  = Wire (UInt(1.W))
          cin := carry
        val gdigit, pdigit,cout,k,l,c1 = Wire (Bits(1.W))
        val a1,b1,g,p,h = Wire(Bits(4.W))//Wire(Vec(4,Bits(1.W)))
        val s = Wire(Vec(4,Bool()))
          a1:=a
          b1:=b
          g:=a1&b1
          p:=a1|b1
          h:=a1^b1
          k := ( p(3) | g(2) ) & ( p(3) | p(1) ) & ( g(3) | p(2) | p(1) )
          l := ( p(3) | p(2) ) & ( p(3) | g(2) | g(1) )
          cout := k | ( l & c1 )
          c1 := ( a(0) & b(0) ) | ( a(0) & cin ) | ( b(0) & cin )
          //  printf("---CLA START---\n")
          //  printf("cin %x\n",cin)
           s(0) := h(0) ^ cin
          //printf("s0 %x\n",s(0).asUInt)

          s(1) := ( h(1) & ~c1 & ~k ) | ( h(1) & c1 & l ) | ( ~h(1) & c1 & ~l ) | ( ~h(1) & ~c1 & k )
          // printf("s1 %x\n",s(1).asUInt)

          s(2) := ( ~p(2) & g(1) ) | ( ~p(3) & h(2) & ~p(1) ) | ( ~p(3) & ~p(2) & p(1) & c1 ) | ( g(2) & g(1) & c1 ) | ( p(3) & p(2) & c1 ) |( g(3) & ~c1 ) | ( h(2) & h(1) & ~c1 )
          // printf("s2 %x\n",s(2).asUInt)
          s(3) := ( g(3) & c1 ) | ( ~h(3) & h(2) & h(1) & c1 ) | ( l & ~k  & ~c1 )
          //  printf("s3 %x\n",s(3).asUInt)

          // a + b = 9, pdigit = 1; a + b >= 10, gdigit = 1;
          gdigit := k | ( l & g(0) );
          pdigit := l & h(0);

        // printf("CLA RESULT a b cin sum carry ,%x,%x,%x,%x, %x\n",a,b, cin,s.asUInt , cout.asUInt)
        // printf("-----CLA END------")
      return (s.asUInt,cout.asUInt)
    } 
   

    // This method takes two 64 bit number  and one one bit number as input and reuten one 64 bit with one carry as output;
    def seventeenDigitCLA (addend_1:Bits, addend_2:Bits) :Bits=
      {
        // val initialCarry = UInt(1.W)
        // initialCarry := 0.U
        val a = Wire (Bits(68.W))
        val b = Wire (Bits(68.W))
        // val sum1,sum2,sum3,sum4,sum5,sum6,sum7,sum8,sum9,sum10,sum11,sum12,sum13,sum14,sum15,sum16,sum17 = Wire(Bits(4.W))
        // val carry1,carry2,carry3,carry4,carry5,carry6,carry7,carry8,carry9,carry10,carry11,carry12,carry13,carry14,carry15,carry16,carry17 = Wire(Bits(1.W))
        val sum = Wire(Bits(68.W))
          a := addend_1
          b := addend_2  
        val(sum1,carry1)  =  CLA (a(3,0),    b(3,0),   0.U)
        val(sum2,carry2)  =  CLA (a(7,4),    b(7,4),   carry1)
        val(sum3,carry3)  =  CLA (a(11,8),   b(11,8),  carry2)
        val(sum4,carry4)  =  CLA (a(15,12),  b(15,12), carry3)
        val(sum5,carry5)  =  CLA (a(19,16),  b(19,16), carry4)
        val(sum6,carry6)  =  CLA (a(23,20),  b(23,20), carry5)
        val(sum7,carry7)  =  CLA (a(27,24),  b(27,24), carry6)
        val(sum8,carry8)  =  CLA (a(31,28),  b(31,28), carry7)
        val(sum9,carry9)  =  CLA (a(35,32),  b(35,32), carry8)
        val(sum10,carry10) =  CLA (a(39,36),  b(39,36), carry9)
        val(sum11,carry11) =  CLA (a(43,40),  b(43,40),carry10)
        val(sum12,carry12) =  CLA (a(47,44),  b(47,44),carry11)
        val(sum13,carry13) =  CLA (a(51,48),  b(51,48),carry12)
        val(sum14,carry14) =  CLA (a(55,52),  b(55,52),carry13)
        val(sum15,carry15) =  CLA (a(59,56),  b(59,56),carry14)
        val(sum16,carry16) =  CLA (a(63,60),  b(63,60),carry15)
        val(sum17,carry17) =  CLA (a(67,64),  b(67,64),carry16)
          // sum := Cat(sum1,sum2 ,sum3,sum4,sum5,sum6,sum7,sum8,sum9,sum10,sum11,sum12,sum13,sum14,sum15,sum16,sum17)
          sum := Cat(sum17,sum16,sum15,sum14,sum13,sum12,sum11,sum10,sum9,sum8,sum7,sum6,sum5,sum4,sum3,sum2,sum1)
          printf(" sum 1-17 are %x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x\n",sum1,sum2 ,sum3,sum4,sum5,sum6,sum7,sum8,sum9,sum10,sum11,sum12,sum13,sum14,sum15,sum16,sum17)
          printf("Seventeen Degit Adder OUTPUT %x, %x, %x \n", a,b,sum)
      return (sum.asUInt)
      //sum16._1.asUInt.asBool
    }
      //val pre1 =  seventeenDigitCLA(multiplicand_X, multiplicand_X)
      // printf("result of addition is %x",pre1)
      // This method takes two 71 bit number  and one one bit number as input and reuten one 71 bit(carry free ) with one carry as output;
    def eighteenDigitCLA (addend_1:Bits, addend_2:Bits) :Bits=
      {
        val a = Wire (Bits(72.W))
        val b = Wire (Bits(72.W))
          a := addend_1
          b:= addend_2
        val sum1 =  seventeenDigitCLA( a(67,0),b(67,0))
        val sum2 = CLA (a(71,68),    b(71,68), a(67))
        //val sum = Cat(sum1,sum2._1)
        val sum = Cat(sum2._1,sum1)
        return (sum.asUInt)
      } 

    def twentyDigitCLA (addend_1:Bits, addend_2:Bits) :Bits=
      {
        val sum = Wire (Bits(80.W))
        val a = Wire (Bits(80.W))
        val b = Wire (Bits(80.W))
          a := addend_1
          b:= addend_2
          printf("incomming of twenty digit is %x, %x",a,b)
          //val sum1 = seventeenDigitCLA( a(67,0),b(67,0))
      val(sum1,carry1)   =  CLA (a(3,0),    b(3,0),   0.U)
      val(sum2,carry2)   =  CLA (a(7,4),    b(7,4),   carry1)
      val(sum3,carry3)   =  CLA (a(11,8),   b(11,8),  carry2)
      val(sum4,carry4)   =  CLA (a(15,12),  b(15,12), carry3)
      val(sum5,carry5)   =  CLA (a(19,16),  b(19,16), carry4)
      val(sum6,carry6)   =  CLA (a(23,20),  b(23,20), carry5)
      val(sum7,carry7)   =  CLA (a(27,24),  b(27,24), carry6)
      val(sum8,carry8)   =  CLA (a(31,28),  b(31,28), carry7)
      val(sum9,carry9)   =  CLA (a(35,32),  b(35,32), carry8)
      val(sum10,carry10) =  CLA (a(39,36),  b(39,36), carry9)
      val(sum11,carry11) =  CLA (a(43,40),  b(43,40),carry10)
      val(sum12,carry12) =  CLA (a(47,44),  b(47,44),carry11)
      val(sum13,carry13) =  CLA (a(51,48),  b(51,48),carry12)
      val(sum14,carry14) =  CLA (a(55,52),  b(55,52),carry13)
      val(sum15,carry15) =  CLA (a(59,56),  b(59,56),carry14)
      val(sum16,carry16) =  CLA (a(63,60),  b(63,60),carry15)
      val(sum17,carry17) =  CLA (a(67,64),  b(67,64),carry16)
      val(sum18,carry18) =  CLA (a(71,68),  b(71,68),carry17)
      val(sum19,carry19) = CLA (a(75,72),   b(75,72),carry18)
      val(sum20,carry20) = CLA (a(79,76),   b(79,76),carry19)
        sum := Cat(sum20,sum19,sum18,sum17,sum16,sum15,sum14,sum13,sum12,sum11,sum10,sum9,sum8,sum7,sum6,sum5,sum4,sum3,sum2,sum1)
        printf(" sum 1-20 are %x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x\n",sum1,sum2 ,sum3,sum4,sum5,sum6,sum7,sum8,sum9,sum10,sum11,sum12,sum13,sum14,sum15,sum16,sum17,sum18,sum19,sum20)
        // val sum = Cat(sum1,sum2._1,sum3._1,sum4._1)
        //al sum = Cat(sum4._1,sum3._1,sum2._1,sum1)
      return (sum.asUInt)
    }

    def twentyfourDigitCLA (addend_1:Bits, addend_2:Bits) :Bits=
      {
        val a = Wire (Bits(96.W))
        val b = Wire (Bits(96.W))
          a := addend_1
          b:= addend_2
        val sum1 =  twentyDigitCLA( a(79,0),b(79,0))
        val sum2 = CLA (a(83,80),    b(83,80), a(79))
        val sum3 = CLA (a(87,84),    b(87,84), a(83))
        val sum4 = CLA (a(91,88),    b(91,88), a(87))
        val sum5 = CLA (a(95,92),    b(95,92), a(91))
        // val sum = Cat(sum1,sum2._1,sum3._1,sum4._1,sum5._1)
        val sum = Cat(sum5._1,sum4._1,sum3._1,sum2._1,sum1)
        return (sum.asUInt)
      }


      // preCoumpute is the value of 1X to 9X This porcess use CLA siquentially
      // and generate . 
      // val preCompute = Reg(Vec(10,Bits(68.W)))
      // val pre1 = Wire(Bits(68.W))
      //preCompute(0) := Bits("b0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000")
      //preCompute(1) := Cat(Bits("b0000"),multiplicand_X)
      //preCompute(2) :=seventeenDigitCLA(preCompute(1), Cat(Bits("b0000"),multiplicand_X))
      //preCompute(3) :=seventeenDigitCLA(preCompute(2), Cat(Bits("b0000"),multiplicand_X))
      //preCompute(4) :=seventeenDigitCLA(preCompute(3), Cat(Bits("b0000"),multiplicand_X))
      //preCompute(5) :=seventeenDigitCLA(preCompute(4), Cat(Bits("b0000"),multiplicand_X))
      //preCompute(6) :=seventeenDigitCLA(preCompute(5), Cat(Bits("b0000"),multiplicand_X))
      //preCompute(7) :=seventeenDigitCLA(preCompute(6), Cat(Bits("b0000"),multiplicand_X))
      //preCompute(8) :=seventeenDigitCLA(preCompute(7), Cat(Bits("b0000"),multiplicand_X))
      //preCompute(9) :=seventeenDigitCLA(preCompute(8), Cat(Bits("b0000"),multiplicand_X))
      //printf("precom %x,%x,%x,%x,%x,%x,%x,%x,%x,%x\n", preCompute(0), preCompute(1), preCompute(2), preCompute(3), preCompute(4), preCompute(5), preCompute(6) ,preCompute(7), preCompute(8),preCompute(9) )

      //======================
  
  val busy = Reg(init = {Bool(false)}) //initialize to false
  val r_recv_max = Reg(UInt(width = xLen));
  val r_cmd_count = Reg(UInt(width = xLen));
  val r_recv_count = Reg(UInt(width = xLen));
  ///val r_resp_rd = Reg(io.resp.bits.rd)
  val r_addr = Reg(UInt(width = xLen))
  // datapath
  val r_total = Reg(UInt(width = xLen));
  val r_tag = Reg(UInt(width = outer.n))//riaz add outer
  val s_idle :: s_mem_acc :: s_finish :: Nil = Enum(Bits(), 3) //FSM
  val r_cmd_state = Reg(UInt(width = 3), init = s_idle) // register withd 3 initail value S_idle
  val r_recv_state = Reg(UInt(width = 3), init = s_idle)
  //when (io.cmd.valid) {
  //  printf("MemTotalExample: On Going. %x, %x\n", r_cmd_state, r_recv_state)
  //  }
  //when (io.cmd.fire()) {
  printf("Accum Mul: Command Received. %x, %x\n", io.cmd.bits.rs1, io.cmd.bits.rs2)
  r_total := UInt(0)
  r_addr := io.cmd.bits.rs1
  r_recv_max := io.cmd.bits.rs2
  r_recv_count := UInt(0)
  r_cmd_count := UInt(0)
  r_tag := UInt(0)
  r_resp_rd := io.cmd.bits.inst.rd
  r_cmd_state := s_mem_acc
  r_recv_state := s_mem_acc
  //}
  io.cmd.ready := (r_cmd_state === s_idle)
  // command resolved if no stalls AND not issuing a load that will need a request
  val cmd_finished = r_cmd_count === r_recv_max
  when ((r_cmd_state === s_mem_acc) && io.mem.req.fire()) 
  {
  printf("AccumMul: IO.MEM Command Received %x %x\n", io.mem.resp.bits.data, r_cmd_state)
  r_cmd_count := r_cmd_count + UInt(1)
  r_tag := r_tag + UInt(1)
  r_addr := r_addr + UInt(8)
  r_cmd_state := Mux(cmd_finished, s_idle, s_mem_acc)
  }
  // MEMORY REQUEST INTERFACE
  io.mem.req.valid := (r_cmd_state === s_mem_acc)
  io.mem.req.bits.addr := r_addr
  io.mem.req.bits.tag := r_tag
  io.mem.req.bits.cmd := M_XRD // perform a load (M_XWR for stores)
  io.mem.req.bits.typ := MT_D // D = 8 bytes, W = 4, H = 2, B = 1
  io.mem.req.bits.data := Bits(0) // we're not performing any stores...
  io.mem.req.bits.phys := Bool(false)
 // io.mem.invalidate_lr := Bool(false)
  val recv_finished = (r_recv_count === r_recv_max)
  when (r_recv_state === s_mem_acc && io.mem.resp.valid) 
  {
  printf("Accum Mul: IO.MEM Received %x %x\n", io.mem.resp.bits.data, r_recv_state)
  //r_total := r_total + io.mem.resp.bits.data
  preCompute(r_recv_count)  :=  io.mem.resp.bits.data
  r_recv_count := r_recv_count + UInt(1)
  r_recv_state := Mux(recv_finished, s_finish, s_mem_acc)
  printf("precom %x,%x,%x,%x,%x,%x,%x,%x,%x,%x\n", preCompute(0), preCompute(1), preCompute(2), preCompute(3), preCompute(4), preCompute(5), preCompute(6) ,preCompute(7), preCompute(8),preCompute(9) )

  }

  // control line
  when (io.mem.req.fire()) {
  busy := Bool(true)
  }
  when ((r_recv_state === s_finish) && io.resp.fire()) {
  r_recv_state := s_idle
  printf("MemTotalExample: Finished. Answer = %x\n", r_total)
  }
  
  ---------------------
     //=================
  

      pp(0) := preCompute(multiplier_Y(3,0))
      pp(1) := preCompute(multiplier_Y(7,4))
      pp(2) := preCompute(multiplier_Y(11,8))
      pp(3) := preCompute(multiplier_Y(15,12))
      pp(4) := preCompute(multiplier_Y(19,16))
      pp(5) := preCompute(multiplier_Y(23,20))
      pp(6) := preCompute(multiplier_Y(27,24))
      pp(7) := preCompute(multiplier_Y(31,28))
      pp(8) := preCompute(multiplier_Y(35,32))
      pp(9) := preCompute(multiplier_Y(39,36))
      pp(10) := preCompute(multiplier_Y(43,40))
      pp(11) := preCompute(multiplier_Y(47,44))
      pp(12) := preCompute(multiplier_Y(51,48))
      pp(13) := preCompute(multiplier_Y(55,52))
      pp(14) := preCompute(multiplier_Y(59,56))
      pp(15) := preCompute(multiplier_Y(63,60))
      printf("pp0-pp14 %x,\n %x\n, %x\n, %x\n, %x\n, %x\n, %x\n, %x\n, %x\n, %x\n, %x\n, %x\n, %x\n, %x\n,%x\n,%x\n",pp(0),pp(1),pp(2),pp(3),pp(4),pp(5),pp(6),pp(7),pp(8),pp(9),pp(10),pp(11),pp(12),pp(13),pp(14),pp(15))

    val level1_1, level1_2, level1_3, level1_4, level1_5, level1_6, level1_7, level1_8 = Wire(Bits(68.W))
    val level2_1, level2_2, level2_3, level2_4                                         = Wire(Bits(72.W))

    val level3_1, level3_2                                                             = Wire(Bits(80.W))
    val level4_1                                                                       = Wire(Bits(96.W))

      level1_1 := seventeenDigitCLA (Cat(Bits("b0000"), pp(0)(67,4).asUInt), pp(1))
    val level1_1_1 = Cat(level1_1,pp(0)(3,0))
      printf("level1_1_1 %x",level1_1_1) 

      level1_2 := seventeenDigitCLA (Cat(Bits("b0000"), pp(2)(67,4).asUInt), pp(3))
    val level1_1_2 = Cat(level1_2,pp(2)(3,0))
      printf("level1_1_2 %x",level1_1_2) 

      level1_3 := seventeenDigitCLA (Cat(Bits("b0000"), pp(4)(67,4).asUInt), pp(5))
    val level1_1_3 = Cat(level1_3,pp(4)(3,0))
      printf("level1_1_3 %x",level1_1_3) 

      level1_4 := seventeenDigitCLA (Cat(Bits("b0000"), pp(6)(67,4).asUInt), pp(7))
    val level1_1_4 = Cat(level1_4,pp(6)(3,0))
      printf("level1_1_4 %x",level1_1_4) 

      level1_5 := seventeenDigitCLA (Cat(Bits("b0000"), pp(8)(67,4).asUInt), pp(9))
    val level1_1_5 = Cat(level1_5,pp(8)(3,0))
      printf("level1_1_5 %x",level1_1_5) 

      level1_6 := seventeenDigitCLA (Cat(Bits("b0000"), pp(10)(67,4).asUInt), pp(11))
    val level1_1_6 = Cat(level1_6,pp(10)(3,0))
      printf("level1_1_6 %x",level1_1_6) 

      level1_7 := seventeenDigitCLA (Cat(Bits("b0000"), pp(12)(67,4).asUInt), pp(13))
    val level1_1_7 = Cat(level1_7,pp(12)(3,0))
      printf("level1_1_7 %x",level1_1_7) 

      level1_8 := seventeenDigitCLA (Cat(Bits("b0000"), pp(14)(67,4).asUInt), pp(15))
    val level1_1_8 = Cat(level1_8,pp(14)(3,0))
      printf("level1_1_8 %x",level1_1_8) 

      //18-digit accumulation
      level2_1 := eighteenDigitCLA (Cat(Bits("b00000000"), level1_1_1(71,8)), level1_1_2)
    val level2_1_1 = Cat(level2_1,level1_1_1(7,0))
      printf("level2_1_1 %x",level2_1_1) 
      level2_2 := eighteenDigitCLA (Cat(Bits("b00000000"), level1_1_3(71,8)), level1_1_4)
    val level2_1_2 = Cat(level2_2,level1_1_3(7,0))
      printf("level2_1_2 %x",level2_1_2) 

      level2_3 := eighteenDigitCLA (Cat(Bits("b00000000"), level1_1_5(71,8)), level1_1_6)
    val level2_1_3 = Cat(level2_3,level1_1_5(7,0))
      printf("level2_1_3 %x",level2_1_3) 

      level2_4 := eighteenDigitCLA (Cat(Bits("b00000000"), level1_1_7(71,8)), level1_1_8)
    val level2_1_4 = Cat(level2_4,level1_1_7(7,0))
      printf("level2_1_4 %x",level2_1_4) 
      // 20 digit accumulation 

      level3_1 := twentyDigitCLA (Cat(Bits("b0000000000000000"), level2_1_1(79,16)), level2_1_2)
    val level3_1_1 = Cat(level3_1,level2_1_1(15,0))
      printf("level3_1_1 %x",level3_1_1) 

      level3_2 := twentyDigitCLA (Cat(Bits("b0000000000000000"), level2_1_3(79,16)), level2_1_4)
    val level3_1_2 = Cat(level3_2,level2_1_3(15,0))
      printf("level3_1_2 %x",level3_1_2) 
      // 24 digit final 

      level4_1 := twentyfourDigitCLA (Cat( Bits("b0000_0000_0000_0000_0000_0000_0000_0000"), level3_1_1(95,32)), level3_1_2)
      //val product = Cat(level4_1,level3_1_1(31,0))
    val finalProduct = Cat(level4_1,level3_1_1(31,0))
      printf("coefficeent multilicatio result is %x",finalProduct)
      //printf("BCD prodcut is %x \n", product)
    
        regfile(addr) :=  finalProduct
        //Method-2 Parallel component this mehotd recieve elevelt emement and return final product as BCD
        //1//val BCD_Procuct = Wire(Bits(127.W))

        printf("end BCD \n") 
  
  
   ////regfile(addr) := wdata // write wadata accumulator // see datapath val accum = regfile(addr)//wdata is stte or MUX output  and  val wdata = Mux(doWrite, addend, accum + addend)
    //  regfile(addr):= BCDConv
  
  
  
  
  
  
  

}

  when (io.mem.resp.valid) 
    {
   printf("Accumulator memory response : res %x\n", io.mem.resp.bits.data)
   regfile(memRespTag) := io.mem.resp.bits.data
   busy(memRespTag) := Bool(false)
  }
  // control
  when (io.mem.req.fire()){
  busy(addr) := Bool(true)
  }

  val doResp = cmd.bits.inst.xd //xd set destinatin register destination register ID
  val stallReg = busy(addr)
  val stallLoad = doLoad && !io.mem.req.ready
  val stallResp = doResp && !io.resp.ready

  cmd.ready := !stallReg && !stallLoad && !stallResp
    // command resolved if no stalls AND not issuing a load that will need a request

  // PROC RESPONSE INTERFACE
  io.resp.valid := cmd.valid && doResp && !stallReg && !stallLoad// && doBCDadd
    // valid response if valid command, need a response, and no stalls
  io.resp.bits.rd := cmd.bits.inst.rd
    // Must respond with the appropriate tag or undefined behavior
 // io.resp.bits.data := accum
  io.resp.bits.data :=accum   // BCDConv

    // Semantics is to always send out prior accumulator register value

  io.busy := cmd.valid || busy.reduce(_||_)
    // Be busy when have pending memory requests or committed possibility of pending requests
  io.interrupt := Bool(false)
    // Set this true to trigger an interrupt on the processor (please refer to supervisor documentation)

  // MEMORY REQUEST INTERFACE
  io.mem.req.valid := cmd.valid && doLoad && !stallReg && !stallResp// && doBCDadd
  io.mem.req.bits.addr := addend
  io.mem.req.bits.tag := addr
  io.mem.req.bits.cmd := M_XRD // perform a load (M_XWR for stores)
  io.mem.req.bits.typ := MT_D // D = 8 bytes, W = 4, H = 2, B = 1
  io.mem.req.bits.data := Bits(0) // we're not performing any stores...
  io.mem.req.bits.phys := Bool(false)
 // printf("Accumulator: Fisnish %x\n", io.resp.bits.data)

}

class  TranslatorExample(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes, nPTWPorts = 1) {
  override lazy val module = new TranslatorExampleModuleImp(this)
}

class TranslatorExampleModuleImp(outer: TranslatorExample)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {
  val req_addr = Reg(UInt(width = coreMaxAddrBits))
  val req_rd = Reg(io.resp.bits.rd)
  val req_offset = req_addr(pgIdxBits - 1, 0)
  val req_vpn = req_addr(coreMaxAddrBits - 1, pgIdxBits)
  val pte = Reg(new PTE)

  val s_idle :: s_ptw_req :: s_ptw_resp :: s_resp :: Nil = Enum(Bits(), 4) //FSM
  val state = Reg(init = s_idle)

  io.cmd.ready := (state === s_idle)

  when (io.cmd.fire()) {
    printf("Translator: On Going")
    req_rd := io.cmd.bits.inst.rd
    req_addr := io.cmd.bits.rs1
    state := s_ptw_req
  }

  private val ptw = io.ptw(0)

  when (ptw.req.fire()) { state := s_ptw_resp }

  when (state === s_ptw_resp && ptw.resp.valid) {
    pte := ptw.resp.bits.pte
    state := s_resp
  }

  when (io.resp.fire()) { state := s_idle }

  ptw.req.valid := (state === s_ptw_req)
  ptw.req.bits.valid := true.B
  ptw.req.bits.bits.addr := req_vpn

  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := req_rd
  io.resp.bits.data := Mux(pte.leaf(), Cat(pte.ppn, req_offset), SInt(-1, xLen).asUInt)

  io.busy := (state =/= s_idle)
  io.interrupt := Bool(false)
  io.mem.req.valid := Bool(false)
}

class  CharacterCountExample(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new CharacterCountExampleModuleImp(this)
  override val atlNode = TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters("CharacterCountRoCC")))))
}

class CharacterCountExampleModuleImp(outer: CharacterCountExample)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
  with HasCoreParameters
  with HasL1CacheParameters {
  val cacheParams = tileParams.icache.get

  private val blockOffset = blockOffBits
  private val beatOffset = log2Up(cacheDataBits/8)

  val needle = Reg(UInt(width = 8))
  val addr = Reg(UInt(width = coreMaxAddrBits))
  val count = Reg(UInt(width = xLen)) 

  val resp_rd = Reg(io.resp.bits.rd)

  val addr_block = addr(coreMaxAddrBits - 1, blockOffset)
  val offset = addr(blockOffset - 1, 0)
  val next_addr = (addr_block + UInt(1)) << UInt(blockOffset)

  val s_idle :: s_acq :: s_gnt :: s_check :: s_resp :: Nil = Enum(Bits(), 5)
  val state = Reg(init = s_idle)

  val (tl_out, edgesOut) = outer.atlNode.out(0)
  val gnt = tl_out.d.bits
  val recv_data = Reg(UInt(width = cacheDataBits))
  val recv_beat = Reg(UInt(width = log2Up(cacheDataBeats+1)), init = UInt(0))

  val data_bytes = Vec.tabulate(cacheDataBits/8) { i => recv_data(8 * (i + 1) - 1, 8 * i) }
  val zero_match = data_bytes.map(_ === UInt(0))
  val needle_match = data_bytes.map(_ === needle)
  val first_zero = PriorityEncoder(zero_match)

  val chars_found = PopCount(needle_match.zipWithIndex.map {
    case (matches, i) =>
      val idx = Cat(recv_beat - UInt(1), UInt(i, beatOffset))
      matches && idx >= offset && UInt(i) <= first_zero
  })
  val zero_found = zero_match.reduce(_ || _)
  val finished = Reg(Bool())

  io.cmd.ready := (state === s_idle)
  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := resp_rd
  io.resp.bits.data := count
  tl_out.a.valid := (state === s_acq)
  tl_out.a.bits := edgesOut.Get(
                       fromSource = UInt(0),
                       toAddress = addr_block << blockOffset,
                       lgSize = UInt(lgCacheBlockBytes))._2
  tl_out.d.ready := (state === s_gnt)

  when (io.cmd.fire()) {
    printf("CharecterCount: On Going")
    addr := io.cmd.bits.rs1
    needle := io.cmd.bits.rs2
    resp_rd := io.cmd.bits.inst.rd
    count := UInt(0)
    finished := Bool(false)
    state := s_acq
  }

  when (tl_out.a.fire()) { state := s_gnt }

  when (tl_out.d.fire()) {
    recv_beat := recv_beat + UInt(1)
    recv_data := gnt.data
    state := s_check
  }

  when (state === s_check) {
    when (!finished) {
      count := count + chars_found
    }
    when (zero_found) { finished := Bool(true) }
    when (recv_beat === UInt(cacheDataBeats)) {
      addr := next_addr
      state := Mux(zero_found || finished, s_resp, s_acq)
    } .otherwise {
      state := s_gnt
    }
  }

  when (io.resp.fire()) { state := s_idle }

  io.busy := (state =/= s_idle)
  io.interrupt := Bool(false)
  io.mem.req.valid := Bool(false)
  // Tie off unused channels
  tl_out.b.ready := Bool(true)
  tl_out.c.valid := Bool(false)
  tl_out.e.valid := Bool(false)
}

 

class MemTotalExample(opcodes: OpcodeSet, val n: Int = 4)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new MemTotalExampleModule(this)
}
class MemTotalExampleModule(outer: MemTotalExample)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
with HasCoreParameters {
  val busy = Reg(init = {Bool(false)}) //initialize to false
  val r_recv_max = Reg(UInt(width = xLen));
  val r_cmd_count = Reg(UInt(width = xLen));
  val r_recv_count = Reg(UInt(width = xLen));
  val r_resp_rd = Reg(io.resp.bits.rd)
  val r_addr = Reg(UInt(width = xLen))
  // datapath
  val r_total = Reg(UInt(width = xLen));
  val r_tag = Reg(UInt(width = outer.n))//riaz add outer
  val s_idle :: s_mem_acc :: s_finish :: Nil = Enum(Bits(), 3) //FSM
  val r_cmd_state = Reg(UInt(width = 3), init = s_idle) // register withd 3 initail value S_idle
  val r_recv_state = Reg(UInt(width = 3), init = s_idle)
  when (io.cmd.valid) {
  printf("MemTotalExample: On Going. %x, %x\n", r_cmd_state, r_recv_state)
  }
  when (io.cmd.fire()) {
  printf("MemTotalExample: Command Received. %x, %x\n", io.cmd.bits.rs1, io.cmd.bits.rs2)
  r_total := UInt(0)
  r_addr := io.cmd.bits.rs1
  r_recv_max := io.cmd.bits.rs2
  r_recv_count := UInt(0)
  r_cmd_count := UInt(0)
  r_tag := UInt(0)
  r_resp_rd := io.cmd.bits.inst.rd
  r_cmd_state := s_mem_acc
  r_recv_state := s_mem_acc
  }
  io.cmd.ready := (r_cmd_state === s_idle)
  // command resolved if no stalls AND not issuing a load that will need a request
  val cmd_finished = r_cmd_count === r_recv_max
  when ((r_cmd_state === s_mem_acc) && io.mem.req.fire()) {
  printf("MemTotalExample: IO.MEM Command Received %x %x\n", io.mem.resp.bits.data, r_cmd_state)
  r_cmd_count := r_cmd_count + UInt(1)
  r_tag := r_tag + UInt(1)
  r_addr := r_addr + UInt(8)
  r_cmd_state := Mux(cmd_finished, s_idle, s_mem_acc)
  }
  // MEMORY REQUEST INTERFACE
  io.mem.req.valid := (r_cmd_state === s_mem_acc)
  io.mem.req.bits.addr := r_addr
  io.mem.req.bits.tag := r_tag
  io.mem.req.bits.cmd := M_XRD // perform a load (M_XWR for stores)
  io.mem.req.bits.typ := MT_D // D = 8 bytes, W = 4, H = 2, B = 1
  io.mem.req.bits.data := Bits(0) // we're not performing any stores...
  io.mem.req.bits.phys := Bool(false)
 // io.mem.invalidate_lr := Bool(false)
  val recv_finished = (r_recv_count === r_recv_max)
  when (r_recv_state === s_mem_acc && io.mem.resp.valid) {
  printf("MemTotalExample: IO.MEM Received %x %x\n", io.mem.resp.bits.data, r_recv_state)
  r_total := r_total + io.mem.resp.bits.data
  r_recv_count := r_recv_count + UInt(1)
  r_recv_state := Mux(recv_finished, s_finish, s_mem_acc)
  }

  // control line
  when (io.mem.req.fire()) {
  busy := Bool(true)
  }
  when ((r_recv_state === s_finish) && io.resp.fire()) {
  r_recv_state := s_idle
  printf("MemTotalExample: Finished. Answer = %x\n", r_total)
  }
  // PROC RESPONSE INTERFACE
   io.resp.valid := (r_recv_state === s_finish)
  // valid response if valid command, need a response, and no stalls
  io.resp.bits.rd := r_resp_rd
  // Must respond with the appropriate tag or undefined behavior
  io.resp.bits.data := r_total
  // Semantics is to always send out prior accumulator register value
  io.busy := io.cmd.valid
  // Be busy when have pending memory requests or committed possibility of pending requests
  io.interrupt := Bool(false)
  // Set this true to trigger an interrupt on the processor (please refer to supervisor documentation)
   }
  


class OpcodeSet(val opcodes: Seq[UInt]) {
  def |(set: OpcodeSet) =
    new OpcodeSet(this.opcodes ++ set.opcodes)

  def matches(oc: UInt) = opcodes.map(_ === oc).reduce(_ || _)
}

object OpcodeSet {
  def custom0 = new OpcodeSet(Seq(Bits("b0001011")))
  def custom1 = new OpcodeSet(Seq(Bits("b0101011")))
  def custom2 = new OpcodeSet(Seq(Bits("b1011011")))
  def custom3 = new OpcodeSet(Seq(Bits("b1111011")))
  def all = custom0 | custom1 | custom2 | custom3
}

class RoccCommandRouter(opcodes: Seq[OpcodeSet])(implicit p: Parameters)
    extends CoreModule()(p) {
  val io = new Bundle {
    val in = Decoupled(new RoCCCommand).flip
    val out = Vec(opcodes.size, Decoupled(new RoCCCommand))
    val busy = Bool(OUTPUT)
  }

  val cmd = Queue(io.in)
  val cmdReadys = io.out.zip(opcodes).map { case (out, opcode) =>
  val me = opcode.matches(cmd.bits.inst.opcode)
    out.valid := cmd.valid && me
    out.bits := cmd.bits
    out.ready && me
  }
  cmd.ready := cmdReadys.reduce(_ || _)
  io.busy := cmd.valid

  assert(PopCount(cmdReadys) <= UInt(1),
    "Custom opcode matched for more than one accelerator")
} 

