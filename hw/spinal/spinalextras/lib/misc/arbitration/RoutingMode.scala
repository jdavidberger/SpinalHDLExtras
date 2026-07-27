package spinalextras.lib.misc.arbitration

// Several components here share the same shape of problem: a resource
// admission/allocation decision is only *exposed* through a register one
// cycle after it's actually decided, even when the decision itself is
// already fully determined the same cycle (FlitRouter's outputNode,
// GrantTableCrossbar's grant matrix). RoutingMode picks how that register
// is handled -- shared across those components so a single NocConfig field
// can drive all of them consistently.
sealed trait RoutingMode

// Original behavior: the decision only takes effect once its register is
// set, so whatever's waiting on a fresh decision pays an unconditional
// 1-cycle bubble even though the decision is already fully determined this
// same cycle.
object Stall extends RoutingMode

// Admit the transfer combinationally, the same cycle the decision is made,
// instead of waiting for the register -- removes the bubble entirely, at
// the cost of a longer combinational path through the component.
object Async extends RoutingMode

// Keep the register (same latency as Stall), but stage the input stream
// through a standard registered Stream pipe (`.stage()`) first, so the
// combinational path feeding the decision register is shorter and the
// staged input's readiness isn't tied directly to the decision logic.
object Register extends RoutingMode
