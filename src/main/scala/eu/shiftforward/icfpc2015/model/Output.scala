package eu.shiftforward.icfpc2015.model

case class Output(problemId: Int,
                  seed: Int,
                  tag: String,
                  solution: List[Command])
