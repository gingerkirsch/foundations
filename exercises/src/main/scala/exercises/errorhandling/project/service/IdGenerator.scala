package exercises.errorhandling.project.service

import exercises.action.async.IO
import exercises.errorhandling.project.OrderId

import java.util.UUID

trait IdGenerator {
  def genOrderId: IO[OrderId]
}

object IdGenerator {
  val random: IdGenerator = new IdGenerator {
    def genOrderId: IO[OrderId] = IO(OrderId(UUID.randomUUID().toString))
  }

  def constant(orderId: OrderId): IdGenerator = new IdGenerator {
    def genOrderId: IO[OrderId] = IO(orderId)
  }
}
