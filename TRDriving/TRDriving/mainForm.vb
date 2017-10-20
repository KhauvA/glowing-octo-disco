Option Explicit On


Public Class mainForm
    Dim myGameThread As System.Threading.Thread


    Private Sub Form1_FormClosed(sender As Object, e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
        If myGameThread Is Nothing = False AndAlso myGameThread.IsAlive Then
            myGameThread.Abort()
            If currentPrinter Is Nothing = False Then currentPrinter.Dispose()
        End If
    End Sub


    Private Sub loadDrivingGame()

        Dim MainGameWindow As New clsGameDrive(0, 0, "game_driving", 10, gameDevices.trackpad, 0, 0, 0, gameScreens.GameDriving, 0, 0)
        MainGameWindow.initialize()
        MainGameWindow.Run(30, 60) '30updates per second, drawing 60frames per second

    End Sub

    Private Sub Button3_Click(sender As System.Object, e As System.EventArgs) Handles Button3.Click
        loadGame(openGLModule.gameDriving)
    End Sub


    Private Sub loadGame(id As openGLModule)
        If myGameThread Is Nothing = False AndAlso myGameThread.IsAlive Then
            myGameThread.Abort()
        End If
        If id = openGLModule.gameDriving Then
            myGameThread = New System.Threading.Thread(AddressOf loadDrivingGame)
            myGameThread.Start()
        End If
    End Sub


End Class
