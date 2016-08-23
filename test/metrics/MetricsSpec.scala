/*
 * Copyright 2016 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package metrics

import play.api.test.Helpers._
import play.api.test._

import org.scalatest.BeforeAndAfterAll
import play.api.test.{FakeRequest, FakeApplication}
import play.api.Play

import uk.gov.hmrc.play.test.UnitSpec

import org.scalatest._
import com.codahale.metrics._
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import java.util.concurrent.TimeUnit

class MetricsSpec extends UnitSpec with MockitoSugar with BeforeAndAfterAll {
  val app = FakeApplication()

  override def beforeAll() {
    Play.start(app)
    super.beforeAll() // To be stackable, must call super.beforeEach
  }

  override def afterAll() {
    try {
      super.afterAll()
    } finally Play.stop()
  }

  trait SharedFixture {
    val mockRegistry = mock[MetricRegistry]
    object MockedMetrics extends GraphiteMetrics {
      override def registry() = mockRegistry
    }
  }

  "Metrics" must {
    "increment on successfulRequest counter" in new SharedFixture {
      // set up
      val counter = new Counter()
      stub(mockRegistry.counter("successful-request")).toReturn(counter)

      // test
      MockedMetrics.successfulRequest

      // check
      counter.getCount() shouldBe 1
    }

    "increment on failedRequest counter" in new SharedFixture {
      // set up
      val counter = new Counter()
      stub(mockRegistry.counter("failed-request")).toReturn(counter)

      // test
      MockedMetrics.failedRequest

      // check
      counter.getCount() shouldBe 1
    }

    "increment on failedCalculation counter" in new SharedFixture {
      // set up
      val counter = new Counter()
      stub(mockRegistry.counter("failed-calculation")).toReturn(counter)

      // test
      MockedMetrics.failedCalculation

      // check
      counter.getCount() shouldBe 1
    }

    "log timing on processRequest" in new SharedFixture {
      // set up
      val timer = new Timer()
      stub(mockRegistry.timer("process-request-timer")).toReturn(timer)

      // test
      MockedMetrics.processRequest(123L, TimeUnit.DAYS)

      // check
      timer.getCount() shouldBe 1
    }

    "log timing on calculationTime" in new SharedFixture {
      // set up
      val timer = new Timer()
      stub(mockRegistry.timer("calculation-timer")).toReturn(timer)

      // test
      MockedMetrics.calculationTime(123L, TimeUnit.DAYS)

      // check
      timer.getCount() shouldBe 1
    }

    "update histograms on tax calculated" in new SharedFixture {
      // set up
      val histogram1 = new Histogram(new SlidingWindowReservoir(5))
      val histogram2 = new Histogram(new SlidingWindowReservoir(5))
      stub(mockRegistry.histogram("tax-calculation")).toReturn(histogram1)
      stub(mockRegistry.histogram("tax-calculation-2016")).toReturn(histogram2)

      // test
      MockedMetrics.taxCalculated("2016", 123L)

      // check
      histogram1.getCount() shouldBe 1
      histogram2.getCount() shouldBe 1
    }

    "update histograms on unused allowance" in new SharedFixture {
      // set up
      val histogram1 = new Histogram(new SlidingWindowReservoir(5))
      val histogram2 = new Histogram(new SlidingWindowReservoir(5))
      stub(mockRegistry.histogram("unused-allowance-calculation")).toReturn(histogram1)
      stub(mockRegistry.histogram("unused-allowance-calculation-2016")).toReturn(histogram2)

      // test
      MockedMetrics.unusedAllowanceCalculated("2016", 123L)

      // check
      histogram1.getCount() shouldBe 1
      histogram2.getCount() shouldBe 1
    }
  }
}
