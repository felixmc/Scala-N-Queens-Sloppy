package com.felixmilea.algorithms

import com.felixmilea.profiling.FunctionTimer
import scala.collection.mutable.ListBuffer

object AttackingQueens extends App
{
	val ft = new FunctionTimer

	for (n <- (1 to 50))
	{
		val board = new ListBuffer [(Int, Int)]
		val solved : Boolean = ft.time { AttackingQueensAlgorithm(n, board) }.asInstanceOf[Boolean]
		
		printf("%d. %s\n", n, if (solved) "Solved" else "Failed")
		
		if (solved)
		{
			printf("Queen Coordinates: %s\nBoard:\n", board.mkString(","))
			printBoard(n, board.toList)
			printf("\nCalculation Time: %dms\n\n", ft.elapsedTime)		  
		}
	}

	def AttackingQueensAlgorithm (n : Int, board : ListBuffer[(Int, Int)], row : Int = 0): Boolean =
	{
		if (row == n)
			return true

		for (col <- (0 to n - 1))
		{
			val location = (col, row)
			
			if (   !checkForQueens(location, n, board.toList, ( 0, -1))  // check top
				&& !checkForQueens(location, n, board.toList, ( 1, -1))  // check top-right diagonal
				&& !checkForQueens(location, n, board.toList, (-1, -1))) // check top-left diagonal
			{
				board += location
				if (AttackingQueensAlgorithm(n, board, row + 1))
					return true
				else
					board -= location
			}
		}
		
		return false
	}
	
	def checkForQueens (startCoord: (Int, Int), n: Int, board : List[(Int, Int)], direction: (Int, Int)) : Boolean =
	{
		def incrementCoord (coord : (Int, Int)) : (Int, Int) = (coord._1 + direction._1, coord._2 + direction._2)

		var foundQueens = false
		var currentCoord = incrementCoord(startCoord)

		while (!foundQueens && (0 until n contains currentCoord._1) && (0 until n contains currentCoord._2))
		{
			if (board.contains(currentCoord)) foundQueens = true
			else currentCoord = incrementCoord(currentCoord)
		}

		return foundQueens
	}
	
	def printBoard (n : Int, board : List[(Int, Int)]) =
	{
		val nOffset = if (n > 9) " " else ""

		// print x-axis labels
		printf("   %s%s\n", nOffset,
		    (1 to n).foldLeft("")((b,a) => {
		    	b + String.format("%d%s%s", a.asInstanceOf[Integer], nOffset, (if (a > 9) "" else " "))
		    }))
	  
		for (x <- (0 to n - 1))
		{
			printf("%s%d ", if (x + 1 > 9) "" else " ", x + 1)
			for (y <- (0 to n - 1))
			{
				printf("%s%s ", nOffset, if (board.contains( (x,y) )) "Q" else "-")
			}
			println
		}
	}

}