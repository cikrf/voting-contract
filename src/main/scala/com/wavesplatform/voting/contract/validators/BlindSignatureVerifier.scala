package com.wavesplatform.voting.contract.validators

import java.math.BigInteger
import java.nio.charset.StandardCharsets.US_ASCII
import java.security.interfaces.RSAPublicKey

import org.bouncycastle.crypto.digests.SHA256Digest

import scala.collection.mutable.ArrayBuffer

object BlindSignatureVerifier {

  case class BlindSigPublicKey(modulo: BigInteger, exp: BigInteger)

  object BlindSigPublicKey {
    def apply(pk: RSAPublicKey): BlindSigPublicKey = new BlindSigPublicKey(pk.getModulus, pk.getPublicExponent)
  }

  def verify(signature: BigInteger, publicKey: BlindSigPublicKey, message: String, hashSize: Int): Boolean = {
    val result = signature.modPow(publicKey.exp, publicKey.modulo)
    val padded = fdh(message.getBytes(US_ASCII), publicKey.modulo, hashSize)
    result == padded
  }

  def fdh(message: Array[Byte], modulo: BigInteger, hashSize: Int): BigInteger = {
    val byteList = new ArrayBuffer[Byte]

    if (hashSize % 256 != 0) {
      throw new RuntimeException("Supported only lengths divisible by 256")
    }

    val numOfBlocks = hashSize / 256
    val hash        = new SHA256Digest()

    Range(0, numOfBlocks).foreach { i =>
      var digest = Array.ofDim[Byte](32)
      val iBytes = toLittleEndian(i)
      hash.update(iBytes, 0, iBytes.length)
      hash.update(message, 0, message.length)
      hash.doFinal(digest, 0)

      if (i == numOfBlocks - 1) {
        var j = 1
        while (fromLittleEndian(byteList.toArray ++ digest).compareTo(modulo) == 1) {
          digest = Array.ofDim[Byte](32)
          val ijBytes = toLittleEndian(i + j)
          hash.update(ijBytes, 0, ijBytes.length)
          hash.update(message, 0, message.length)
          hash.doFinal(digest, 0)
          j += 1
        }
      }

      byteList ++= digest
    }

    fromLittleEndian(byteList.toArray)
  }

  private def fromLittleEndian(bytes: Array[Byte]): BigInteger = {
    new BigInteger(1, bytes.reverse)
  }

  private def toLittleEndian(value: Int): Array[Byte] = {
    val bytes = new Array[Byte](4)
    bytes(0) = (value >> (0 * 8) & 0xff).toByte
    bytes(1) = (value >> (1 * 8) & 0xff).toByte
    bytes(2) = (value >> (2 * 8) & 0xff).toByte
    bytes(3) = (value >> (3 * 8) & 0xff).toByte
    bytes
  }
}
