
object Main extends App {
    var board: Array[Array[Int]] = Array.ofDim(11, 11)
    var playerCurrentId = 10;

    object CaseState {
        val Empty = 0
        val Enemy = 1
        val Bomb = 2
        val Wall = 3
    }

    val caseStateDefinition: Array[String] = Array("Empty", "Enemy", "Bomb", "Wall", "Player1", "Player2")

    object Constants {
        val BombDelay = 10
        val bombDefaultPower = 1
        val startBombNumber = 1
    }

    class Player(name: String) {
        var id = playerCurrentId
        playerCurrentId += 1
        var bombNumber = 0
        var bombMaxNumber = Constants.startBombNumber
        var bombPower = Constants.bombDefaultPower
    }

    class Bomb(lines: Int, column: Int, originPlayer: Player) {
        private var lifePoint: Int = Constants.BombDelay
        var i: Int = lines;
        var j : Int= column;
        val power: Int = originPlayer.bombPower
        var player: Player = originPlayer;

        board(i)(j) = CaseState.Bomb

        def tick(): Unit = {
            this.lifePoint -= 1
        }

        def explode(): Unit = {
            bombExplode(this)
        }
    }
    

    def printBoard(a: Array[Array[Int]]) = {
        for(i <- 0 until a.length) {
            for(j <- 0 until a(i).length) {
                print(a(i)(j) + " ")
            }
            print("\n")
        }
    }

    def getPlayerCurrentCaseIndexes(player: Player): Array[Int] = {
        var indexes: Array[Int] = Array.ofDim(2)
        for(i <- 0 until board.length; j <- 0 until board(i).length) {
            if(board(i)(j) == player.id) {
                indexes(0) = i
                indexes(1) = j
            }
        }
        return indexes
    }

    def checkMoveIsAtRange(i: Int, j: Int, playerCurrentCell: Array[Int]): Boolean = {
        // Checking if dest cells is at range of player
        if(i != playerCurrentCell(0) && j != playerCurrentCell(1)) {
            // Dest cell is not on the same line and not on the same column as the source
            return false;
        }
        if(i == playerCurrentCell(0)) {
            if(j < playerCurrentCell(1) - 1 || j > playerCurrentCell(1) + 1) {
                // Dest cell is on the same line but further that 1 cell from the source
                return false;
            }
        }
        if(j == playerCurrentCell(1)) {
            if(i < playerCurrentCell(0) - 1 || i > playerCurrentCell(0) + 1) {
                return false;
            }
        }
        return true;
    }

    /**
      * 
      * Move a player to a cell
      * @param i - the line index of the dest cell
      * @param j - The column index of the dest cell
      * @param player - The int of the player to move
      * @return - True if player has moved, false otherwise
      */
    def movePlayer(i: Int, j: Int, player: Player): Boolean = {
        var playerCurrentCell = getPlayerCurrentCaseIndexes(player)
        if(i < 0 || board.length - 1 < i) {
            return false;
        }
        if(j < 0 || board(0).length - 1 < j) {
            return false;
        }
        // i and j are corrects

        if(!checkMoveIsAtRange(i, j, playerCurrentCell)) {
            return false;
        }
        var caseState:Int = board(i)(j);
        if (caseState == CaseState.Empty) {
            board(playerCurrentCell(0))(playerCurrentCell(1)) = CaseState.Empty
            board(i)(j) = player.id
            return true;
        } else {
            // Player can't move on this case because it's not empty
            return false;
        }
        return false;
    }

    def dropBomb(i: Int, j: Int, player: Player): Boolean = {
        var cell: Int = board(i)(j)
        if(cell != player.id) {
            // Can't drop a bomb on a cell you're not on
            return false;
        }
        if(player.bombNumber >= player.bombMaxNumber) {
            return false;
        }
        var bomb: Bomb = new Bomb(i, j, player);
        player.bombNumber += 1
        return true;
    }

    def bombExplode(bomb: Bomb) = {
        var verticalStart = bomb.i - bomb.power
        var verticalEnd = bomb.i + bomb.power
        var horizontalStart = bomb.j - bomb.power
        var horizontalEnd = bomb.j + bomb.power
        // checking indexes
        if(verticalStart < 0) {
            verticalStart = 0
        }
        if (verticalEnd >= board.length) {
            verticalEnd = board.length - 1
        }
        if(horizontalStart < 0) {
            horizontalStart = 0
        }
        if (horizontalEnd >= board(bomb.i).length) {
            horizontalEnd = board(bomb.i).length - 1
        }
    }
}