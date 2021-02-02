package com.riskified.toolkit.test

import java.net.{ServerSocket, Socket}

import com.riskified.toolkit.test.RandomPort.randomFreePort
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

class RandomPortSpec extends FlatSpec with Matchers {
  val beAFailure =
    Matcher { (t: Try[_]) =>
      MatchResult(
        t.isFailure,
        s"$t is not a failed try",
        s"$t is a failed try"
      )
    }

  "RandomFreePort" should "return a free port" in {
    val port = randomFreePort().get

    val tryListen = Try(new Socket("127.0.0.1", port))
    tryListen.foreach(_.close())

    tryListen should beAFailure
  }

  it should "fail if no ports free" in {
    val port = 12345
    val socket = new ServerSocket(port)

    try randomFreePort(minPort = port, maxPort = port) should beAFailure
    finally socket.close()
  }
}
