package spinalextras.lib.misc

import spinal.core.Component

trait ComponentWithKnownLatency extends Component {
  def latency(): Int

  def pipelineCyclesPerElement(): Int = 1
}