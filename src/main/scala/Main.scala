import hevs.graphics.FunGraphics
import java.awt.Color
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import scala.collection.mutable.ListBuffer

object Main extends App {
    var boardDims = 10
    var board: Array[Array[Int]] = Array.ofDim(boardDims, boardDims)
    var playerCurrentId = 10;
    var players: Array[Player] = Array.ofDim(2)
    var bombs: ListBuffer[Bomb] = new ListBuffer[Bomb]()

    // object Directions {
    //     val UP = 0
    //     val RIGHT = 1
    //     val DOWN = 2
    //     val LEFT = 3
    // }
    object ObjectsColor {
        val Empty = Color.white
        val Wall = Color.black
        val Bomb = Color.gray
        val Player = Color.blue
    }
    object CaseState {
        val Empty = 0
        val Enemy = 1
        val Bomb = 2
        val Wall = 3
    }
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
        var dead = false

        def die() = {
            removePlayerFromBoard(this.id)
            this.dead = true
            checkVictory()
        }
    }

    def checkVictory(): Unit = {
        var count: Int = 0
        var player: Player = players(0)
        for(i <- players.indices) {
            if(!players(i).dead) {
                count += 1
                player = players(i)
            }
        }
        if(count == 1) {
            // Player has won
        }
    }

    def removePlayerFromBoard(playerId: Int) : Unit = {
        for(i <- 0 until board.length; j <- 0 until board(i).length) {
            if(board(i)(j) == playerId) {
                board(i)(j) = 0
            }
        }
    }

    def doGameplayTick(): Unit = {
        println("Doing gameplay tick")
        var toRemove: ListBuffer[Int] = new ListBuffer[Int]()
        var bombList = bombs.toList
        println("bombs length : " + bombs.length)
        for(i <- bombList.indices) {
            bombList(i).tick()
            println("Ticking bomb, lifepoint : " + bombList(i).lifePoint)
            if(bombList(i).lifePoint <= 0) {
                bombList(i).explode()
                toRemove.addOne(i)
            }
        }
        if(toRemove.length == 0) {
            return
        }
        for(i <- toRemove(toRemove.length - 1) to 0) {
            bombs.remove(toRemove(i))
        }
    }

    class Bomb(lines: Int, column: Int, originPlayer: Player) {
        var lifePoint: Int = Constants.BombDelay
        var i: Int = lines;
        var j : Int= column;
        val power: Int = originPlayer.bombPower
        var player: Player = originPlayer;


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

    def getBombOnCurrentCell(i: Int, j: Int): Option[Bomb] = {
        var lBombs: List[Bomb] = bombs.toList
        for(c <- lBombs.indices) {
            if(lBombs(c).i == i && lBombs(c).j == j) {
                var bombToReturn = lBombs(c)
                bombs.remove(bombs.indexOf(lBombs(c)))
                return Some(lBombs(c))
            }
        }
        return None
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
        var optBombCell = getBombOnCurrentCell(playerCurrentCell(0), playerCurrentCell(1))
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
            board(i)(j) = player.id
            if(optBombCell != None) {
                board(playerCurrentCell(0))(playerCurrentCell(1)) = CaseState.Bomb
            } else {
                board(playerCurrentCell(0))(playerCurrentCell(1)) = CaseState.Empty
            }
            doGameplayTick()
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
        bombs = bombs.addOne(bomb)
        player.bombNumber += 1
        return true;
    }

    def getPlayer(playerId: Number): Option[Player] = {
        for(i <- players.indices) {
            if(players(i).id == playerId) {
                return Some(players(i))
            }
        }
        return None
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

        // Indexes are in the bounds of the board
        // Horizontal part
        
        def checkCase(currentCase: Int): Unit = {
                if(currentCase >= 10) {
                    // Case contain a player
                    var res: Option[Player] = getPlayer(currentCase)
                    if(res == None) {
                        println("ERROR: Found an id of player wich was not registered in the players array")
                        return
                    }
                    var player: Player = res.get
                    player.die()
                }
        }

        def horizontal(): Unit = {
            var lineIndex = bomb.i
            for(j <- horizontalStart to horizontalEnd) {
                var currentCase = board(lineIndex)(j)
                checkCase(currentCase)
            }
        }
        def vertical() : Unit = {
            var columnIndex = bomb.j
            for(i <- verticalStart to verticalEnd) {
                var currentCase = board(i)(columnIndex)
                checkCase(currentCase)
            }
        }
        board(bomb.i)(bomb.j) = CaseState.Empty
    }

    def start() = {
        var displaySize = 2500
        var display: FunGraphics = FunGraphics(displaySize, displaySize)
        def createBoard() = {
            val boardType = 0
            // Simple board
            if(boardType == 0) {
                for(i <- board.indices; j <- board(i).indices) {
                    if(i == 0 || i == board.length - 1 || j == 0 || j == board(i).length - 1) {
                        board(i)(j) = CaseState.Wall
                    }
                }
                players(0) = new Player("First player")
                board(1)(2) = players(0).id
            }
        }
        
        def printBoardToDisplay() = {
            var elementSize = displaySize / boardDims
            def printRect(xStart: Int, xEnd: Int, yStart: Int, yEnd: Int, color: Color) = {
                for(i <- xStart until xEnd; j <- yStart until yEnd) {
                    display.setPixel(i, j, color)
                }
            }
            def printWall(i: Int, j: Int) = {
                printRect(j * elementSize, (j + 1) * elementSize, i * elementSize, (i + 1) * elementSize, ObjectsColor.Wall)
            }
            def printPlayer(i: Int, j: Int) = {
                printRect(j * elementSize, (j + 1) * elementSize, i * elementSize, (i + 1) * elementSize, ObjectsColor.Player)
            }
            def printBackground(i: Int, j: Int) = {
                printRect(j * elementSize, (j + 1) * elementSize, i * elementSize,(i + 1) * elementSize, ObjectsColor.Empty)
            }
            def printBomb(i: Int, j: Int) = {
                printRect(j * elementSize, (j + 1) * elementSize, i * elementSize, (i + 1) * elementSize, ObjectsColor.Bomb)
            }
            for(i <- board.indices; j <- board.indices) {
                var cCase = board(i)(j)
                cCase match {
                    case CaseState.Empty => printBackground(i ,j)
                    case CaseState.Wall => printWall(i, j)
                    case CaseState.Bomb => printBomb(i, j)
                    case _ => printPlayer(i, j)
                }
            }
        }
        createBoard()
        printBoardToDisplay()

        display.setKeyManager(new KeyAdapter() { // Will be called when a key has been pressed
            override def keyPressed(e: KeyEvent): Unit = {
                var keyChar:Int = e.getKeyCode()
                keyChar match {
                    case KeyEvent.VK_LEFT => {
                        var cCase = getPlayerCurrentCaseIndexes(players(0))
                        cCase(1) = cCase(1) - 1
                        movePlayer(cCase(0), cCase(1), players(0))
                        printBoardToDisplay()
                    }
                    case KeyEvent.VK_RIGHT => {
                        var cCase = getPlayerCurrentCaseIndexes(players(0))
                        cCase(1) = cCase(1) + 1
                        movePlayer(cCase(0), cCase(1), players(0))
                        printBoardToDisplay()
                    }
                    case KeyEvent.VK_UP => {
                        var cCase = getPlayerCurrentCaseIndexes(players(0))
                        cCase(0) = cCase(0) - 1
                        movePlayer(cCase(0), cCase(1), players(0))
                        printBoardToDisplay()
                    }
                    case KeyEvent.VK_DOWN => {
                        var cCase = getPlayerCurrentCaseIndexes(players(0))
                        cCase(0) = cCase(0) + 1
                        movePlayer(cCase(0), cCase(1), players(0))
                        printBoardToDisplay()
                    }
                    case KeyEvent.VK_CONTROL => {
                        var cCase = getPlayerCurrentCaseIndexes(players(0))
                        dropBomb(cCase(0), cCase(1), players(0))
                    }
                    case _ => {
                        println("You pressed a non assigned key " + keyChar)
                    }
                }
            }
        })

    }

    start()
}