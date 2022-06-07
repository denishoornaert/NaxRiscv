package naxriscv.riscv

import spinal.core._

object Rvfd {
  import FloatRegFile._

//  def FMV_W_X            = M"111100000000-----000-----1010011"
//  def FADD_S             = M"0000000------------------1010011"
//  def FSUB_S             = M"0000100------------------1010011"
//  def FMUL_S             = M"0001000------------------1010011"
//  def FDIV_S             = M"0001100------------------1010011"
//  def FSGNJ_S            = M"0010000----------000-----1010011"
//  def FSGNJN_S           = M"0010000----------001-----1010011"
//  def FSGNJX_S           = M"0010000----------010-----1010011"
//  def FMIN_S             = M"0010100----------000-----1010011"
//  def FMAX_S             = M"0010100----------001-----1010011"
//  def FSQRT_S            = M"010110000000-------------1010011"
//  def FCVT_S_W           = M"110100000000-------------1010011"
//  def FCVT_S_WU          = M"110100000001-------------1010011"
//  def FCVT_S_L           = M"110100000010-------------1010011"
//  def FCVT_S_LU          = M"110100000011-------------1010011"
//  def FCVT_W_S           = M"110000000000-------------1010011"
//  def FCVT_WU_S          = M"110000000001-------------1010011"
//  def FCVT_L_S           = M"110000000010-------------1010011"
//  def FCVT_LU_S          = M"110000000011-------------1010011"
//  def FCLASS_S           = M"111000000000-----001-----1010011"
//  def FMADD_S            = M"-----00------------------1000011"
//  def FMSUB_S            = M"-----00------------------1000111"
//  def FNMSUB_S           = M"-----00------------------1001011"
//  def FNMADD_S           = M"-----00------------------1001111"
//
//  def FLE_S              = M"1010000----------000-----1010011"
//  def FLT_S              = M"1010000----------001-----1010011"
//  def FEQ_S              = M"1010000----------010-----1010011"
  def FADD_D             = TypeR(M"0000001------------------1010011")
  def FSUB_D             = TypeR(M"0000101------------------1010011")
  def FMUL_D             = TypeR(M"0001001------------------1010011")
  def FDIV_D             = TypeR(M"0001101------------------1010011")
//  def FSGNJ_D            = M"0010001----------000-----1010011"
//  def FSGNJN_D           = M"0010001----------001-----1010011"
//  def FSGNJX_D           = M"0010001----------010-----1010011"
//  def FMIN_D             = M"0010101----------000-----1010011"
//  def FMAX_D             = M"0010101----------001-----1010011"
//  def FSQRT_D            = M"010110100000-------------1010011"
  def FMV_X_W            = TypeF2I(M"111000000000-----000-----1010011")
//  def FCVT_W_D           = M"110000100000-------------1010011"
//  def FCVT_WU_D          = M"110000100001-------------1010011"
//  def FCVT_L_D           = M"110000100010-------------1010011"
//  def FCVT_LU_D          = M"110000100011-------------1010011"
  def FMV_X_D            = TypeF2I(M"111000100000-----000-----1010011")
//  def FCLASS_D           = M"111000100000-----001-----1010011"
//  def FCVT_D_W           = M"110100100000-------------1010011"
//  def FCVT_D_WU          = M"110100100001-------------1010011"
//  def FCVT_D_L           = M"110100100010-------------1010011"
//  def FCVT_D_LU          = M"110100100011-------------1010011"
//  def FMV_D_X            = M"111100100000-----000-----1010011"
  def FMADD_D            = TypeR3(M"-----01------------------1000011")
//  def FMSUB_D            = M"-----01------------------1000111"
//  def FNMSUB_D           = M"-----01------------------1001011"
//  def FNMADD_D           = M"-----01------------------1001111"
//  def FLE_D              = M"1010001----------000-----1010011"
//  def FLT_D              = M"1010001----------001-----1010011"
//  def FEQ_D              = M"1010001----------010-----1010011"
//
//  def FCVT_S_D           = M"010000000001-------------1010011"
//  def FCVT_D_S           = M"010000100000-------------1010011"
//

  def FLW                = TypeILQ(M"-----------------010-----0000111")
  def FLD                = TypeILQ(M"-----------------011-----0000111")
  def FSW                = TypeSSQ(M"-----------------010-----0100111")
  def FSD                = TypeSSQ(M"-----------------011-----0100111")
}
