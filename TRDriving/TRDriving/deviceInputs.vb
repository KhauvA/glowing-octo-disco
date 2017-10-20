Public Class deviceInputs
   
    Public Property trackpad_X As Integer = 0
    Public Property trackpad_Y As Integer = 0


    Private oldX As Single = 0
    Private oldY As Single = 0
    Private oldStored As Boolean = False

    Private currentChosenDevice As Integer = -999

    Public Sub resetDeviceSelection()
        oldStored = False
        currentChosenDevice = -999
    End Sub


    Public Sub getInput(ByVal inputMethod As gameDevices, ByRef reps As Single)

        Dim distanceTravelled As Single = 0

        If (currentChosenDevice <> inputMethod) Then
            currentChosenDevice = inputMethod
            oldStored = False
        End If


        Select Case inputMethod
            
            Case gameDevices.trackpad
                Dim tempX As Single = Cursor.Position.X / SCREENWIDTH
                Dim tempY As Single = Cursor.Position.Y / SCREENHEIGHT



                If tempX < 0 Then
                    tempX = 0
                ElseIf tempX > 1 Then
                    tempX = 1
                End If

                If tempY < 0 Then
                    tempY = 0
                ElseIf tempY > 1 Then
                    tempY = 1
                End If

                GlobalRotationCapped = tempX * 360



                GlobalCurrentX = tempX
                GlobalCurrentY = tempY

                If oldStored = False Then
                    oldStored = True
                    oldX = GlobalCurrentX * SCREENWIDTH
                    oldY = GlobalCurrentY * SCREENHEIGHT
                Else
                    distanceTravelled = Math.Pow(Math.Pow(GlobalCurrentX * SCREENWIDTH - oldX, 2) + Math.Pow(GlobalCurrentY * SCREENHEIGHT - oldY, 2), 0.5)
                    reps = reps + distanceTravelled
                    oldX = GlobalCurrentX * SCREENWIDTH
                    oldY = GlobalCurrentY * SCREENHEIGHT
                End If


            
        End Select
    End Sub
End Class
