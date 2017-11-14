Option Explicit On
Imports OpenTK.Graphics.OpenGL


Public Module Module1

    Public Const SCREENWIDTH As Integer = 1920
    Public Const SCREENHEIGHT As Integer = 1080

    Public quadratic As IntPtr = IntPtr.Zero

    Public GlobalCurrentX As Single = 0
    Public GlobalCurrentY As Single = 0
    Public GlobalRotationCapped As Single = 0


    Public currentPrinter As OpenTK.Graphics.TextPrinter
    Private currentFont_Arial_144 As Font = New Font("Arial", 144)
    Private currentFont_Arial_100 As Font = New Font("Arial", 100)
    Private currentFont_Arial_72 As Font = New Font("Arial", 72)
    Private currentFont_Arial_60 As Font = New Font("Arial", 60)
    Private currentFont_Arial_48 As Font = New Font("Arial", 48)
    Private currentFont_Arial_36 As Font = New Font("Arial", 36)
    Private currentFont_Arial_30 As Font = New Font("Arial", 30)
    Private currentFont_Arial_18 As Font = New Font("Arial", 18)
    Private cf As Font

    Public soundContext As OpenTK.Audio.AudioContext = New OpenTK.Audio.AudioContext
    Public soundBuffer(9) As Integer
    Public soundSource(9) As Integer

    Public inputMethods As New deviceInputs


    Public startUpPath As String = Application.StartupPath

    Public Enum gameDevices
        trackpad = 7
    End Enum

    Public Enum fontType
        arial
    End Enum

    Public Enum openGLModule
        gameDriving = 0
    End Enum

    Public Enum gameScreens
        GameDriving
    End Enum

    Public Enum fontSizes
        size_144 = 144
        size_100 = 100
        size_72 = 72
        size_60 = 60
        size_48 = 48
        size_36 = 36
        size_30 = 30
        size_18 = 18
    End Enum

    Public Sub initializeNewGameDevices(ByVal im As gameDevices)
        inputMethods.resetDeviceSelection()
    End Sub


    Public Function Rand(ByVal low As Long, ByVal high As Long) As Long
        Randomize()
        Dim r As Long
        r = ((high - low + 1) * Rnd()) + low
        If r > high Then r = high
        If r < low Then r = low
        Rand = r
    End Function

    Public Sub playSound(ByVal filename1 As String)

        Static soundCounter As Integer = 0


        Dim sound1 As OpenTK.Audio.AudioReader = New OpenTK.Audio.AudioReader(filename1)

        Try
            OpenTK.Audio.AL.SourceStop(soundSource(soundCounter))
            OpenTK.Audio.AL.DeleteSource(soundSource(soundCounter))
            OpenTK.Audio.AL.DeleteBuffer(soundBuffer(soundCounter))
        Catch ex As Exception


        End Try

        soundBuffer(soundCounter) = OpenTK.Audio.AL.GenBuffer
        soundSource(soundCounter) = OpenTK.Audio.AL.GenSource

        OpenTK.Audio.AL.BufferData(soundBuffer(soundCounter), sound1.ReadToEnd())
        OpenTK.Audio.AL.Source(soundSource(soundCounter), OpenTK.Audio.ALSourcei.Buffer, soundBuffer(soundCounter))
        OpenTK.Audio.AL.SourcePlay(soundSource(soundCounter))

        sound1.Dispose()
        sound1 = Nothing


        If soundCounter < soundBuffer.Length - 1 Then
            soundCounter += 1
        Else
            soundCounter = 0
        End If
    End Sub

    Public Sub drawTextAt(ByRef text As String, ByVal a As Single, ByVal r As Single, ByVal g As Single, ByVal b As Single, ByVal x As Single, ByVal y As Single, ByVal size As fontSizes, ByVal ft As fontType, Optional ByVal scale As Single = 1)

        GL.PushAttrib(AttribMask.AllAttribBits)
        GL.PushMatrix()

        GL.Disable(EnableCap.Lighting)

        GL.BlendFunc(BlendingFactorSrc.SrcAlpha, BlendingFactorDest.One)
        GL.Enable(EnableCap.Blend)
        GL.LoadIdentity()


        currentPrinter.Begin()
        GL.Translate(x * (1 / scale), y * (1 / scale), 0)
        GL.Scale(scale, scale, scale)

        Select Case ft
            Case fontType.arial
                If size = fontSizes.size_18 Then
                    currentPrinter.Print(text, currentFont_Arial_18, Color.FromArgb(a, r, g, b))
                ElseIf size = fontSizes.size_144 Then
                    currentPrinter.Print(text, currentFont_Arial_144, Color.FromArgb(a, r, g, b))
                ElseIf size = fontSizes.size_100 Then
                    currentPrinter.Print(text, currentFont_Arial_100, Color.FromArgb(a, r, g, b))
                ElseIf size = fontSizes.size_72 Then
                    currentPrinter.Print(text, currentFont_Arial_72, Color.FromArgb(a, r, g, b))
                ElseIf size = fontSizes.size_60 Then
                    currentPrinter.Print(text, currentFont_Arial_60, Color.FromArgb(a, r, g, b))
                ElseIf size = fontSizes.size_48 Then
                    currentPrinter.Print(text, currentFont_Arial_48, Color.FromArgb(a, r, g, b))
                ElseIf size = fontSizes.size_36 Then
                    currentPrinter.Print(text, currentFont_Arial_36, Color.FromArgb(a, r, g, b))
                ElseIf size = fontSizes.size_30 Then
                    currentPrinter.Print(text, currentFont_Arial_30, Color.FromArgb(a, r, g, b))
                End If
            
        End Select






        GL.Scale(1, 1, 1)
        currentPrinter.End()

        GL.PopMatrix()
        GL.PopAttrib()



    End Sub

    Public Sub drawScoreAndTime(ByRef score As Integer, ByRef totaltime As Integer, ByVal r As Single, ByVal g As Single, ByVal b As Single, ByVal size As Integer, ByVal a As Integer)


        Select Case size
            Case fontSizes.size_144
                cf = currentFont_Arial_144
            Case fontSizes.size_100
                cf = currentFont_Arial_100
            Case fontSizes.size_72
                cf = currentFont_Arial_72
            Case fontSizes.size_60
                cf = currentFont_Arial_60
            Case fontSizes.size_48
                cf = currentFont_Arial_48
            Case fontSizes.size_36
                cf = currentFont_Arial_36
            Case fontSizes.size_30
                cf = currentFont_Arial_30
            Case fontSizes.size_18
                cf = currentFont_Arial_18
        End Select



        GL.PushAttrib(AttribMask.AllAttribBits)
        GL.BlendFunc(BlendingFactorSrc.SrcAlpha, BlendingFactorDest.One)
        GL.Enable(EnableCap.Blend)
        GL.Disable(EnableCap.Lighting)


        GL.LoadIdentity()
        currentPrinter.Begin()
        GL.Translate(20, 20, 0)
        currentPrinter.Print("Score: " & score, cf, Color.FromArgb(a, r, g, b))
        GL.Translate(1600, 0, 0)
        currentPrinter.Print("Time: " & totaltime, cf, Color.FromArgb(a, r, g, b))
        currentPrinter.End()

        GL.PopAttrib()
    End Sub

End Module
