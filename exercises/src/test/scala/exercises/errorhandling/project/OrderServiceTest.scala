package exercises.errorhandling.project

import exercises.errorhandling.project.OrderGenerator.{instantGen, orderIdGen}
import exercises.errorhandling.project.service.{Clock, IdGenerator, OrderService, OrderStore}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class OrderServiceTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("createOrder inserts a new order in the store") {
    forAll(orderIdGen, instantGen) { (orderId, now) =>
      val orderStore  = OrderStore.inMemory()
      val clock       = Clock.constant(now)
      val idGenerator = IdGenerator.constant(orderId)
      val service     = new OrderService(orderStore, clock, idGenerator)

      val workflow = for {
        newOrder <- service.createNewOrder
        fetched  <- orderStore.get(newOrder.id)
      } yield fetched

      assert(workflow.unsafeRun().isDefined)
    }
  }

}
