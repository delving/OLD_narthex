//===========================================================================
//    Copyright 2014 Delving B.V.
//
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
//===========================================================================

package services

import java.io._
import scala.language.postfixOps
import play.api.libs.json._

object ActorMessages {

  case class AnalyzeThese(jobs: List[(File, FileRepo)])

  case class Analyze(file: File, fileRepo: FileRepo)

  case class AnalysisProgress(percent: Int, fileRepo: FileRepo)

  case class AnalysisTreeComplete(json: JsValue, fileRepo: FileRepo)

  case class AnalysisError(file: File, fileRepo: FileRepo)

  case class AnalysisComplete(fileRepo: FileRepo)

  case class SortType(ordering: Ordering[String])

  object SortType {
    val VALUE_SORT = SortType(Ordering[String])
    val HISTOGRAM_SORT: SortType = SortType(Ordering[String].reverse)
  }

  case class Sort(nodeRepo: NodeRepo, sortType: SortType)

  case class Sorted(nodeRepo: NodeRepo, sortedFile: File, sortType: SortType)

  case class Count(nodeRepo: NodeRepo)

  case class Counted(nodeRepo: NodeRepo, uniqueCount: Int, sampleFiles: Seq[Int])

  case class Merge(nodeRepo: NodeRepo, inFileA: File, inFileB: File, mergeResultFile: File, sortType: SortType)

  case class Merged(merge: Merge, fileA: File, sortType: SortType)
  
  case class SaveRecords(fileRepo: FileRepo, recordRoot: String, uniqueId: String)

  case class RecordsSaved(fileRepo: FileRepo)

}

