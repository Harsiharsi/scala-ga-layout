package layout


object TypeAlias {
  type PhysicalKey = String
  type AssignableChar = String
  type Chromosome = Map[PhysicalKey, AssignableChar]
  type ChromosomeTuple = Map[PhysicalKey, (AssignableChar, AssignableChar)]
  type Ngram = String

  type Chunk = String
  type Interchunk = String
  type NonChunk = String
  type HandAction = List[Chunk]
}
