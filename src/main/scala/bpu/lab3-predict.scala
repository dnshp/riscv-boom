package boom

import Chisel._
import freechips.rocketchip.config.{Parameters, Field}

case class Lab3Parameters(
  enabled: Boolean = true,
  history_length: Int = 1,
  info_size: Int = 0)

case object Lab3Key extends Field[Lab3Parameters]

class Lab3BrPredictor(
    fetch_width: Int,
    history_length: Int)(implicit p: Parameters)
      extends BrPredictor(fetch_width, history_length)(p)
{
  // -----------------------------------------------------
  // Initialize all tables
  // -----------------------------------------------------

  val lht = Mem(1024, UInt(history_length.W))

  val lpt = Mem(1 << history_length, UInt(3.W))

  val pathhist = Reg(UInt(width = history_length))

  val gpt = Mem(1 << history_length, UInt(2.W))

  val cpt = Mem(1 << history_length, UInt(2.W))

  val stall = !io.resp.ready

  // -----------------------------------------------------
  // Get local prediction
  // -----------------------------------------------------

  val pc = io.req_pc
  val lht_idx = pc >> 2
  val localhist = RegEnable(lht(lht_idx), !stall)
  val localpred = lpt.read(localhist)

  // -----------------------------------------------------
  // Predict branch direction
  // -----------------------------------------------------
  
  val globalpred = gpt.read(pathhist)
  val choicepred = cpt.read(pathhist)
  io.resp.bits.takens := Mux(choicepred(1), globalpred(1), localpred(2))
  io.resp.valid := !this.disable_bpd
  val info = Cat(lht_idx, pathhist)
  io.resp.bits.info := RegNext(info)

  // -----------------------------------------------------
  // Get info from commit
  // -----------------------------------------------------

  val commit_en = this.commit.valid
  val commit_lht_idx = RegEnable(this.commit.bits.info.info(30 + history_length - 1, history_length), commit_en)
  val commit_pathhist = RegEnable(this.commit.bits.info.info(history_length - 1, 0), commit_en)
  val commit_taken = RegEnable(this.commit.bits.ctrl.taken(0), commit_en)

  // -----------------------------------------------------
  // Get previous states from tables
  // -----------------------------------------------------

  val commit_localhist = lht.read(commit_lht_idx)
  val commit_localpred = lpt.read(commit_localhist)
  val commit_globalpred = gpt.read(commit_pathhist)
  val commit_choicepred = cpt.read(commit_pathhist)

  // -----------------------------------------------------
  // Calculate update values
  // -----------------------------------------------------

  val update_en = RegNext(commit_en)
  val commit_localhist_update = Cat(commit_localhist(history_length - 2, 0), commit_taken)
  val commit_localpred_update = Mux(commit_taken, 
  	Mux(commit_localpred === "b111".U, commit_localpred, commit_localpred + 1.U), 
  	Mux(commit_localpred === "b000".U, commit_localpred, commit_localpred - 1.U))
  val commit_globalpred_update = Mux(commit_taken,
  	Mux(commit_globalpred === "b11".U, commit_globalpred, commit_globalpred + 1.U),
  	Mux(commit_globalpred === "b00".U, commit_globalpred, commit_globalpred - 1.U))
  val commit_choicepred_update = Mux(commit_localpred(2) === commit_globalpred(1), commit_choicepred, 
  	Mux(commit_taken === commit_globalpred(1), 
  		Mux(commit_choicepred === "b11".U, commit_choicepred, commit_choicepred + 1.U), 
  		Mux(commit_choicepred === "b00".U, commit_choicepred, commit_choicepred - 1.U)))

  // -----------------------------------------------------
  // Update tables
  // -----------------------------------------------------

  when (update_en) {
    lht.write(commit_lht_idx, commit_localhist_update)
    lpt.write(commit_localhist, commit_localpred_update)
    gpt.write(commit_pathhist, commit_globalpred_update)
    cpt.write(commit_pathhist, commit_choicepred_update)
    pathhist := (pathhist << 1) + commit_taken
  }
}
