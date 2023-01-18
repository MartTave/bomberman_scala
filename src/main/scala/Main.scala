import hevs.graphics.FunGraphics
import java.awt.Color
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.collection.immutable
import com.sourcegraph.semanticdb_javac.Semanticdb.Constant
import hevs.graphics.utils.GraphicsBitmap


object Timer {
  def apply(interval: Int, repeats: Boolean = true)(op: => Unit) {
    val timeOut = new javax.swing.AbstractAction() {
      def actionPerformed(e : java.awt.event.ActionEvent) = op
    }
    val t = new javax.swing.Timer(interval, timeOut)
    t.setRepeats(repeats)
    t.start()
  }
}

object Main extends App {
    var waitingForRestart: Boolean = false
    var board: Array[Array[Int]] = Array.ofDim(Constants.BoardDims, Constants.BoardDims)
    var playerCurrentId = 10;
    var players: ListBuffer[Player] = new ListBuffer()
    var bombs: ListBuffer[Bomb] = new ListBuffer[Bomb]()
    var powerUpTiming = Constants.TicksBetweenPowerUpSpawn


    object ObjectsColor {
        val Empty = Color.white
        val Wall = Color.black
        val Bomb = Color.ORANGE
        val BombNbrUp = Color.green
        val BombPowerUp = Color.pink
        val DestructibleWall = Color.lightGray
        val Explosion = Color.red
    }
    object CaseState {
        val Empty = 0
        val Enemy = 1
        val Bomb = 2
        val Wall = 3
        val BombMaxPowerUp = 4
        val BombPowerPowerUp = 5
        val DestructibleWall = 6
    }
    object Constants {
        val BoardDims = 25
        val BombDelay = 6 //3 sec
        val bombDefaultPower = 1
        val startBombNumber = 1
        val MaxPowerUpOnBoard = 5
        val TicksBetweenPowerUpSpawn = 20 // 10s
        val DisplaySize = 1000
        val ElementSize = this.DisplaySize / this.BoardDims
        val Display: FunGraphics = FunGraphics(this.DisplaySize, this.DisplaySize)
        val PowerUpSize = this.ElementSize / 2
        val ChanceForDestructibleWallGeneration = 1.5
        val PowerUpOnBlockDestructionChance = 5
    }

    class Player(playerName: String, playerColor: Color) {
        var name: String = playerName
        var id = playerCurrentId
        playerCurrentId += 1
        var bombNumber = 0
        var bombMaxNumber = Constants.startBombNumber
        var bombPower = Constants.bombDefaultPower
        var dead = false
        var color:Color = playerColor

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
            println("Player has won !")
            // Player has won
            printMessage(player.name + " has won ! Press r to restart")
            players.remove(0, players.length)
            println("Player length : " + players.length)
        } else if (count == 0) {
            // Draw
            printMessage("This is a draw ! Press r to restart")
            players.remove(0, players.length)
            println("Player length : " + players.length)
        }
    }

    def printMessage(msg: String) = {
        waitingForRestart = true
        Constants.Display.setColor(Color.white)
        Constants.Display.drawFillRect(0, 0, Constants.DisplaySize, Constants.DisplaySize)
        Constants.Display.drawString(Constants.DisplaySize / 4, Constants.DisplaySize / 2, msg, Color.black, Constants.DisplaySize / 40)
    }

    def removePlayerFromBoard(playerId: Int) : Unit = {
        for(i <- 0 until board.length; j <- 0 until board(i).length) {
            if(board(i)(j) == playerId) {
                board(i)(j) = 0
            }
        }
    }

    def doGameplayTick(): Unit = {
        if(waitingForRestart) {
            return
        }
        powerUpTiming -= 1
        if(powerUpTiming <= 0) {
            // We need to spawn a power up and reset the timing
            spawnPowerUp()
            powerUpTiming = Constants.TicksBetweenPowerUpSpawn
        }
        var bombList = bombs.toList
        for(i <- bombList.indices) {
            bombList(i).tick()
            if(bombList(i).lifePoint <= 0) {
                bombList(i).explode()
            }
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
            if(this.lifePoint > 0) {
                Timer(100, false) {
                    printCircle(this.i, this.j, Color.yellow, Constants.ElementSize)
                }
            }
            printCell(i, j)
        }

        def explode(): Unit = {
            def removeBomb(): Unit = {
                for(i <- bombs.indices) {
                    if(bombs(i) == this) {
                        bombs.remove(i)
                        return
                    }
                }
            }
            removeBomb()
            bombExplode(this)
            player.bombNumber -= 1
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
                //bombs.remove(bombs.indexOf(lBombs(c)))
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
        if (caseState == CaseState.Empty || caseState == CaseState.BombMaxPowerUp || caseState == CaseState.BombPowerPowerUp) {
            board(i)(j) = player.id
            if(caseState == CaseState.BombPowerPowerUp) {
                player.bombPower += 1
                println("Power of bombs has increasead !")
            }
            if(caseState == CaseState.BombMaxPowerUp) {
                player.bombMaxNumber += 1
                println("Max number of bombs has increased !")
            }
            if(optBombCell != None) {
                board(playerCurrentCell(0))(playerCurrentCell(1)) = CaseState.Bomb
            } else {
                board(playerCurrentCell(0))(playerCurrentCell(1)) = CaseState.Empty
            }
            return true;
        } else {
            // Player can't move on this case because it's not empty
            return false;
        }
        return false;
    }

    def spawnPowerUp(): Unit = {
        var powerUpType: Boolean = Math.random() * 2 > 1
        // If true, spawn a max bomb nbr power up. Spawn bomb power up otherwise

        def getPowerUpNbr(): Int = {
            var nbr = 0
            for(i <- board.indices;j <- board(i).indices) {
                if(board(i)(j) == CaseState.BombMaxPowerUp || board(i)(j) == CaseState.BombPowerPowerUp) {
                    nbr += 1
                }
            }
            return nbr
        }
        def findCell() = {
            var foundCell = false
            var lineIndexes: ListBuffer[Int] = board.indices to ListBuffer
            lineIndexes = Random.shuffle(lineIndexes)
            // Here, we randomise the lineIndexes in order to spawn a buff in a random line
            // We now need to pass through each line and randomize it.
            for(i <- lineIndexes) {
                var column = board(i).indices to ListBuffer
                column = Random.shuffle(column)
                for(j <- column) {
                    var cell = board(i)(j)
                    if(!foundCell && cell == CaseState.Empty) {
                        // We have found an empty cell, we can spawn a power up
                        foundCell = true
                        if(powerUpType) {
                            board(i)(j) = CaseState.BombMaxPowerUp
                        } else {
                            board(i)(j) = CaseState.BombPowerPowerUp
                        }
                        printCell(i, j)
                    }
                }
            }
        }
        if(getPowerUpNbr() < Constants.MaxPowerUpOnBoard) {
            findCell()
        }
    }

    def dropBomb(i: Int, j: Int, player: Player): Boolean = {
        var cell: Int = board(i)(j)
        if(cell != player.id) {
            // Can't drop a bomb on a cell you're not on
            println("Trying to drop bomb on a cell your not on")
            return false;
        }
        if(player.bombNumber >= player.bombMaxNumber) {
            println("Trying to drop a bomb past the bomb limit for this player")
            return false;
        }
        var bomb: Bomb = new Bomb(i, j, player);
        bombs.insert(0, bomb)
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
        var cellsToRefresh: ListBuffer[Array[Int]] = new ListBuffer()
        cellsToRefresh.addOne(Array(bomb.i, bomb.j))
        board(bomb.i)(bomb.j) = CaseState.Empty
        printRect(bomb.i, bomb.j, ObjectsColor.Explosion)
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
        
        def checkCase(i: Int, j: Int): Boolean = {
            var currentCase = board(i)(j)
            if(currentCase == CaseState.Wall) {
                return true
            } else if (currentCase == CaseState.DestructibleWall) {
                board(i)(j) = CaseState.Empty
                if(Math.random() * Constants.PowerUpOnBlockDestructionChance > Constants.PowerUpOnBlockDestructionChance - 1) {
                    if(Math.random() * 2 > 1) {
                        board(i)(j) = CaseState.BombMaxPowerUp
                    } else {
                        board(i)(j) = CaseState.BombPowerPowerUp
                    }
                }
                printRect(i, j, ObjectsColor.Explosion)
                cellsToRefresh.addOne(Array[Int](i, j))
                return true
            } else {
                printRect(i, j, ObjectsColor.Explosion)
                cellsToRefresh.addOne(Array[Int](i, j))
                if(currentCase >= 10) {
                    // Case contain a player
                    var res: Option[Player] = getPlayer(currentCase)
                    if(res == None) {
                        println("ERROR: Found an id of player wich was not registered in the players array")
                    } else {
                        var player: Player = res.get
                        player.die()
                    }
                } else if (currentCase == CaseState.Bomb) {
                    println("Explosion chain !" + bombs.length)
                    for(b <- bombs.indices) {
                        var c = bombs(b)
                        if(c.i == i && c.j == j) {
                            // Found the bomb
                            c.explode()
                        }
                    }
                }
                return false
            }
        }

        def horizontal(): Unit = {
            var lineIndex: Int = bomb.i
            var wallEncountered: Boolean = false
            for(j <- (bomb.j - 1) to horizontalStart by -1) {
                if(!wallEncountered) {
                    wallEncountered = checkCase(lineIndex, j)
                }
            }
            wallEncountered = false
            for(j <- (bomb.j + 1) to horizontalEnd) {
                if(!wallEncountered) {
                    wallEncountered = checkCase(lineIndex, j)
                }
            }
        }
        def vertical() : Unit = {
            var columnIndex = bomb.j
            var wallEncountered: Boolean = false
            for(i <- (bomb.i - 1) to verticalStart by -1) {
                if(!wallEncountered) {
                    wallEncountered = checkCase(i, columnIndex)
                }
            }
            wallEncountered = false
            for(i <- (bomb.i + 1) to verticalEnd) {
                if(!wallEncountered) {
                    wallEncountered = checkCase(i, columnIndex)
                }
            }
        }
        horizontal()
        vertical()
        Timer(200, false) {
            for(i <- cellsToRefresh.indices) {
                printCell(cellsToRefresh(i)(0), cellsToRefresh(i)(1))
            }
        }
    }

    def createBoard() = {
        val boardType = 2
        // Simple board
        if(boardType == 0) {
            for(i <- board.indices; j <- board(i).indices) {
                if(i == 0 || i == board.length - 1 || j == 0 || j == board(i).length - 1) {
                    board(i)(j) = CaseState.Wall
                }
            }
            players(0) = new Player("First player", Color.blue)
            board(1)(2) = players(0).id
        }
        // Simple double player board
        if(boardType == 1) {
            for(i <- board.indices; j <- board(i).indices) {
                if(i == 0 || i == board.length - 1 || j == 0 || j == board(i).length - 1) {
                    board(i)(j) = CaseState.Wall
                }
            }
            players.addOne(new Player("First player", Color.blue))
            players.addOne(new Player("Second player", Color.red))
            board(3)(4) = CaseState.Wall
            board(7)(7) = CaseState.DestructibleWall
            board(1)(1) = players(0).id
            board(4)(4) = players(1).id
        }
        // Full board !!!
        if(boardType == 2) {
            println("Generating a full board !")
            def createDestructibleWall(i: Int, j: Int, chanceModifier: Double = 0) = {
                var finalChance = Constants.ChanceForDestructibleWallGeneration + chanceModifier
                if(finalChance < 1) {
                    finalChance = 1
                }
                if(Math.random() * finalChance > finalChance - 1) {
                    board(i)(j) = CaseState.DestructibleWall
                }
            }
            for(i <- board.indices) {
                if((i + 1) % 2 == 0) {
                    for(j <- board(i).indices) {
                        if((j + 1) % 2 == 0) {
                            board(i)(j) = CaseState.Wall
                        } else {
                            createDestructibleWall(i, j)
                        }
                    }
                } else {
                    for(j <- board(i).indices) {
                        if((j + 1) % 2 == 0) {
                            createDestructibleWall(i, j)
                        } else {
                            createDestructibleWall(i, j, .5)
                        }
                    }
                }
            }
            players.addOne(new Player("Blue", Color.blue))
            board(0)(0) = players(0).id
            board(1)(0) = CaseState.Empty
            board(0)(1) = CaseState.Empty
            players.addOne(new Player("Purple player", Color.MAGENTA))
            board(board.length - 1)(board(board.length - 1).length - 1) = players(1).id
            board(board.length - 2)(board(board.length - 2).length - 1) = CaseState.Empty
            board(board.length - 1)(board(board.length - 1).length - 2) = CaseState.Empty
        }
    }

    def printRect(i: Int, j: Int, color: Color) = {
        val xStart = j * Constants.ElementSize
        val xEnd = (j + 1) * Constants.ElementSize
        val yStart = i * Constants.ElementSize
        val yEnd = (i + 1) * Constants.ElementSize
        for(i <- xStart until xEnd; j <- yStart until yEnd) {
            Constants.Display.setPixel(i, j, color)
        }
    }
    def printCircle(i: Int, j: Int, color: Color, size: Int = Constants.PowerUpSize) = {
        val x = j * Constants.ElementSize + (Constants.ElementSize / 2) - size / 2
        val y = i * Constants.ElementSize + (Constants.ElementSize / 2) - size / 2
        Constants.Display.setColor(color)
        Constants.Display.drawFilledCircle(x, y, size)
    }
    def printCell(i: Int, j: Int) = {
        var cCase = board(i)(j)
        cCase match {
            case CaseState.Empty => printBackground(i ,j)
            case CaseState.Wall => printWall(i, j)
            case CaseState.Bomb => printBomb(i, j)
            case CaseState.BombMaxPowerUp => printMaxBombUp(i, j)
            case CaseState.BombPowerPowerUp => printBombPowerUp(i, j)
            case CaseState.DestructibleWall => printDestructibleWall(i, j)
            case _ => {
                var found:Boolean = false
                for(p <- players.indices) {
                    if(players(p).id == cCase) {
                        printPlayer(i, j, players(p))
                        found = true
                    }
                }
                if(!found) {
                    println("Couldn't find what to print for index : " + cCase)
                }
            }
        }
    }
    def printWall(i: Int, j: Int) = {
        printRect(i, j, ObjectsColor.Wall)
    }
    def printPlayer(i: Int, j: Int, player: Player) = {
        printRect(i, j, player.color)
    }
    def printBackground(i: Int, j: Int) = {
        printRect(i, j, ObjectsColor.Empty)
    }
    def printBomb(i: Int, j: Int) = {
        printRect(i, j, ObjectsColor.Empty)
        printCircle(i, j, ObjectsColor.Bomb, Constants.ElementSize)
    }
    def printMaxBombUp(i: Int, j: Int) = {
        printRect(i, j, ObjectsColor.Empty)
        printCircle(i, j, ObjectsColor.BombNbrUp)
    }
    def printBombPowerUp(i: Int, j: Int) = {
        printRect(i, j, ObjectsColor.Empty)
        printCircle(i, j, ObjectsColor.BombPowerUp)
    }
    def printDestructibleWall(i: Int, j: Int) = {
        printRect(i, j, ObjectsColor.DestructibleWall)
    }
    def printBoardToDisplay() = {
        for(i <- board.indices; j <- board.indices) {
            printCell(i, j)
        }
    }
    
    object Direction {
        val UP = 0
        val RIGHT = 1
        val DOWN = 2
        val LEFT = 3
    }

    def directionKeyPressed(playerId: Int, direction: Int): Unit = {
        if(playerId < 0) {
            println("Please give a valid number")
            return
        } else if (playerId > players.length - 1) {
            println("You're trying to move a player that's not in the game !")
            return
        }
        var cCase = getPlayerCurrentCaseIndexes(players(playerId))
        var oldPosition = getPlayerCurrentCaseIndexes(players(playerId))
        direction match {
            case Direction.LEFT => {
                cCase(1) = cCase(1) - 1
            }
            case Direction.RIGHT => {
                cCase(1) = cCase(1) + 1
            }
            case Direction.DOWN => {
                cCase(0) = cCase(0) + 1
            }
            case Direction.UP => {
                cCase(0) = cCase(0) - 1
            }
        }
        if(movePlayer(cCase(0), cCase(1), players(playerId))) {
            printCell(oldPosition(0), oldPosition(1))
            printCell(cCase(0), cCase(1))
        }
    }

    def start() = {
        createBoard()
        printBoardToDisplay()
    }

    Constants.Display.setKeyManager(new KeyAdapter() { // Will be called when a key has been pressed
        override def keyPressed(e: KeyEvent): Unit = {
            var keyChar:Int = e.getKeyCode()
            keyChar match {
                case KeyEvent.VK_LEFT => {
                    directionKeyPressed(0, Direction.LEFT)
                }
                case KeyEvent.VK_RIGHT => {
                    directionKeyPressed(0, Direction.RIGHT)
                }
                case KeyEvent.VK_UP => {
                    directionKeyPressed(0, Direction.UP)
                }
                case KeyEvent.VK_DOWN => {
                    directionKeyPressed(0, Direction.DOWN)
                }
                case KeyEvent.VK_CONTROL => {
                    var cCase = getPlayerCurrentCaseIndexes(players(0))
                    dropBomb(cCase(0), cCase(1), players(0))
                }
                case KeyEvent.VK_W => {
                    directionKeyPressed(1, Direction.UP)
                }
                case KeyEvent.VK_A => {
                    directionKeyPressed(1, Direction.LEFT)
                }
                case KeyEvent.VK_S => {
                    directionKeyPressed(1, Direction.DOWN)
                }
                case KeyEvent.VK_D => {
                    directionKeyPressed(1, Direction.RIGHT)
                }
                case KeyEvent.VK_SPACE => {
                    if(players.length - 1 >= 1) {
                        var cCase = getPlayerCurrentCaseIndexes(players(1))
                        dropBomb(cCase(0), cCase(1), players(1))
                    }
                }
                case KeyEvent.VK_R => {
                    if(waitingForRestart) {
                        waitingForRestart = false
                        start()
                    }
                }
                case _ => {
                    println("You pressed a non assigned key " + keyChar)
                }
            }
        }
    })
    Timer(500) {
        // Every 500 ms
        doGameplayTick()
    }
    start()
    //Constants.Display.drawTransformedPicture(0, 0, 0, 1, "../../../brickwall.jpg")
}