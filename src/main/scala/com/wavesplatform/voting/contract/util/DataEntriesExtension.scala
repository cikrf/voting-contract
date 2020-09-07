package com.wavesplatform.voting.contract.util

import java.math.BigInteger

import cats.implicits._
import com.google.protobuf.ByteString
import com.wavesplatform.protobuf.common.DataEntry
import com.wavesplatform.voting.contract.VotingError
import com.wavesplatform.voting.contract.VotingError.{GenericVotingError, UnknownEnumParamValue}

object DataEntriesExtension {

  implicit class DataEntriesExtensionMethods(val value: Seq[DataEntry]) extends AnyVal {

    private def findParamIndex(paramName: String): Either[VotingError, Int] = {
      val index = value.indexWhere(_.key == paramName)
      Either.cond(
        index != -1 && !value(index).value.isEmpty,
        index,
        VotingError.RequiredParamIsMissing(paramName)
      )
    }

    def extractBooleanParam(paramName: String): Either[VotingError, Boolean] = {
      for {
        index <- findParamIndex(paramName)
        paramValue = value(index).value.boolValue
        result <- Either.cond(
          paramValue.nonEmpty,
          paramValue.get,
          VotingError.RequiredParamValueMissing(paramName)
        )
      } yield result
    }

    def extractLongParam(paramName: String): Either[VotingError, Long] = {
      for {
        index <- findParamIndex(paramName)
        paramValue = value(index).value.intValue
        result <- Either.cond(
          paramValue.nonEmpty,
          paramValue.get,
          VotingError.RequiredParamValueMissing(paramName)
        )
      } yield result
    }

    def extractBinaryParam(paramName: String): Either[VotingError, ByteString] = {
      for {
        index <- findParamIndex(paramName)
        paramValue = value(index).value.binaryValue
        result <- Either.cond(
          paramValue.nonEmpty,
          paramValue.get,
          VotingError.RequiredParamValueMissing(paramName)
        )
      } yield result
    }

    def extractStringParam(paramName: String): Either[VotingError, String] = {
      for {
        index <- findParamIndex(paramName)
        paramValue = value(index).value.stringValue
        result <- Either.cond(
          paramValue.exists(!_.isBlank),
          paramValue.get,
          VotingError.RequiredParamValueMissing(paramName)
        )
      } yield result
    }

    def extractEnumParam[T <: enumeratum.EnumEntry](
      paramName: String,
      enumType: enumeratum.Enum[T]): Either[VotingError, T] = {
      for {
        valueStr <- extractStringParam(paramName)
        result   <- enumType.withNameInsensitiveEither(valueStr).leftMap(_ => UnknownEnumParamValue(paramName, valueStr))
      } yield result
    }

    def extractBigIntegerParam(paramName: String): Either[VotingError, BigInteger] = {
      for {
        valueStr <- extractStringParam(paramName)
        result <- Either
          .catchNonFatal(new BigInteger(valueStr, 16))
          .leftMap(_ =>
            GenericVotingError(s"Parameter '$paramName' value is not hexadecimal big integer; found '$valueStr'"))
      } yield result
    }

    private def extractParamOpt(paramName: String): Option[DataEntry] = {
      val index = value.indexWhere(_.key == paramName)
      Either.cond(index >= 0, value(index), None).toOption
    }

    def extractOptionalStringParam(paramName: String): Option[String] = {
      val paramValueOpt = extractParamOpt(paramName)
      val paramValue    = paramValueOpt.flatMap(_.value.stringValue)
      paramValue
    }
  }
}
