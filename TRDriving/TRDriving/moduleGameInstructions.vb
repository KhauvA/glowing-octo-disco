Public Class moduleGameInstructions

    Public Shared Function returnPrompt(ByVal g As gameScreens, ByVal duration As Integer, ByVal im As gameDevices) As String
        Dim returnValue As String = ""
        Select Case g

            Case gameScreens.GameDriving
                If im = gameDevices.trackpad Then
                    returnValue = "For this game, you will be using the touchpad. " & vbCrLf & _
                    "Move your finger along the sensor left and right to rotate the car." & vbCrLf & _
                    "Drive through the colored cones to score points."
                End If


        End Select




        returnValue = returnValue & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & _
            vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & _
            "Press spacebar to start."

        Return returnValue


    End Function







End Class
