package common

import java.io.{IOException, ObjectInputStream, ObjectOutputStream}
import java.net.{ServerSocket, Socket}

import scala.util.Random

object Util {

    // Try to bind socket to port
    def tryBindSocket(portRangeStart: Int, portRangeWidth: Int, maxAttempts: Int = 100): ServerSocket = {
        val randGen = new Random()
        var attemptsCnt: Int = 0

        while (attemptsCnt < maxAttempts) {
            attemptsCnt += 1

            try {
                return new ServerSocket(portRangeStart + randGen.nextInt(portRangeWidth))
            } catch {
                // Don't print anything, this is expected
                case e: IOException => //e.printStackTrace()
            }
        }

        throw new Exception("Couldn't bind port...")
    }

//    def pingNode(nodeConn: Connection): Boolean = {
//        val toOtherNode: Socket = new Socket(nodeConn.ipAddress, nodeConn.port)
//
//        val out = new ObjectOutputStream(toOtherNode.getOutputStream)
//        val in = new ObjectInputStream(toOtherNode.getInputStream)
//
//        // Ping-pong
//        out.writeObject("Ping")
//        out.flush()
//
//        val pingResponse: String = in.readObject().asInstanceOf[String]
//
//        // Close the socket
//        toOtherNode.close()
//
//        // Return whether ping-pong went thru
//        pingResponse.equals("Pong")
//    }

//    def checkIsConnected(nodeConn: Connection): Boolean = {
//        val toOtherNode: Socket = new Socket(nodeConn.ipAddress, nodeConn.port)
//
//        val out = new ObjectOutputStream(toOtherNode.getOutputStream)
//        val in = new ObjectInputStream(toOtherNode.getInputStream)
//
//        // Check if connected
//        out.writeObject("AreYouConnected")
//        out.flush()
//
//        val connectedResponse: Boolean = in.readObject().asInstanceOf[Boolean]
//
//        // Close the socket
//        toOtherNode.close()
//
//        connectedResponse
//    }

    def isPowerOfTwo(num: Int): Boolean = num > 0 && (num & -num) == num


    /**
      * Get first power of two that is >= n
      */
    def getSmallestPow2GreaterThanOrEqN(n: Int): Int = {
        // Assume int is 4 bytes

        var _n = n

        if (_n < 0) {
            return 0
        }

        _n -= 1
        _n |= _n >> 1
        _n |= _n >> 2
        _n |= _n >> 4
        _n |= _n >> 8
        _n |= _n >> 16

        _n + 1
    }

    def log2floor(num: Int): Int = (Math.log(num) / Math.log(2)).toInt
}