Option Explicit On

Imports OpenTK.Graphics.OpenGL

Public MustInherit Class clsGame
    Inherits OpenTK.GameWindow
    Public GlLoaded As Boolean = False


    Public completed As Boolean = False

    Public currentInputMethod As gameDevices
    Public currentGameScreen As gameScreens

    Public currentID As Integer
    Public currentName As String

    Public needToTakePic As Boolean = False
    Public takePicTime As Date


    Public started As Boolean = False
    Protected recorded As Boolean = False
    Protected displayedResult As Boolean = False
    Protected buttonPresses As Integer
    Protected previousHighScore As Integer
    Protected startTime As Date
    Protected prompt As String
    Protected currentOption1 As Integer
    Protected currentOption2 As Integer
    Protected currentOption3 As Integer


    Protected totalTime As Integer = 180
    Protected elapsedTime As Integer = 0
    Protected score As String = 0
    Protected surveyAnswers As String
    Protected surveyScore As Integer = 0
    Protected reps As Integer = 0
    Protected textureNames() As String

    Private startsec As Date

    '****************************************************************************************************************************************
    'create members for textures
    '****************************************************************************************************************************************
    Private base_NUM_TEXTURES As Integer        'stores the number of textures
    Private base_MyTexture() As Integer         'stores the location id for the textures
    Private base_TextureString() As String      'stores the location of the textures, used when textures loaded by path
    Private base_TextureImage() As Image        'stores the location of the textures, used when textures loaded by embedded resource
    Private base_EmbeddedTextures As Boolean    'indicates whether or not textures are loaded by path or by embedded resource


    '****************************************************************************************************************************************
    ' all inherited classes must create these functions spedific to their own game
    '****************************************************************************************************************************************
    Public MustOverride Sub handleKeyPress(ByVal sender As Object, ByVal e As OpenTK.KeyPressEventArgs) Handles Me.KeyPress
    Public MustOverride Sub handleKeyUp(ByVal k As Keys)
    Public MustOverride Sub initialize()
    Public MustOverride Sub renderLoop()
    Public MustOverride Sub setViewPort()



    Protected Property NUM_TEXTURES As Integer
        Get
            Return base_NUM_TEXTURES
        End Get
        Private Set(ByVal value As Integer)
            base_NUM_TEXTURES = value
        End Set
    End Property

    Protected Property MyTexture() As Integer()
        Get
            Return base_MyTexture
        End Get
        Private Set(ByVal value As Integer())
            base_MyTexture = value
        End Set
    End Property



    Protected Sub loadTextures()
        If NUM_TEXTURES > 0 Then
            Dim Image As Bitmap
            Dim bitmapdata As System.Drawing.Imaging.BitmapData


            GL.Enable(EnableCap.Texture2D)
            GL.GenTextures(NUM_TEXTURES, MyTexture)

            Dim i As Integer
            For i = 0 To NUM_TEXTURES - 1
                If base_EmbeddedTextures = False Then
                    Image = New Bitmap(base_TextureString(i))
                Else
                    Image = New Bitmap(base_TextureImage(i))
                End If


                bitmapdata = Image.LockBits(New Rectangle(0, 0, Image.Width, Image.Height), System.Drawing.Imaging.ImageLockMode.ReadOnly, Drawing.Imaging.PixelFormat.Format24bppRgb)
                GL.BindTexture(TextureTarget.Texture2D, MyTexture(i))
                GL.TexImage2D(EnableCap.Texture2D, 0, PixelInternalFormat.Rgb8,
                                    Image.Width, Image.Height, 0, 32992,
                                    PixelType.UnsignedByte, bitmapdata.Scan0)
                GL.TexParameterI(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, 9729)   '// Linear Filtering
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, 9729)   '// Linear Filtering
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, 9729)     '// Linear Filtering
                Image.UnlockBits(bitmapdata)
                Image.Dispose()
                Image = Nothing

            Next

            GL.Disable(EnableCap.Texture2D)
        End If
    End Sub



    Protected Sub specifyTextures(ByVal textures() As String)
        ReDim base_TextureString(base_NUM_TEXTURES - 1)
        base_TextureString = textures
        base_EmbeddedTextures = False
    End Sub

    Protected Sub specifyTextures(ByVal textures() As Image)
        ReDim base_TextureImage(base_NUM_TEXTURES - 1)
        base_TextureImage = textures
        base_EmbeddedTextures = True
    End Sub

    Protected Sub decrementTime()
        If startsec.AddSeconds(1) < Now Then
            totalTime = totalTime - 1
            elapsedTime = elapsedTime + 1
            startsec = Now

        End If
    End Sub

    Protected Sub decrementTimeDecrementVar(ByRef secondaryTime As Integer)
        If startsec.AddSeconds(1) < Now Then
            totalTime = totalTime - 1
            elapsedTime = elapsedTime + 1
            startsec = Now

            secondaryTime = secondaryTime - 1
        End If
    End Sub

    Protected Sub decrementTimeCheckMyo(ByVal inputMethod As gameDevices)
        If startsec.AddSeconds(1) < Now Then
            totalTime = totalTime - 1
            elapsedTime = elapsedTime + 1
            startsec = Now

        End If
    End Sub






    Protected Sub start()
        If started = False Then
            reps = 0

            initializeNewGameDevices(currentInputMethod)
            started = True
            startsec = Now
        End If
    End Sub

    Protected Sub drawInput()

        GL.PushAttrib(AttribMask.AllAttribBits)
        GL.PushMatrix()
        GL.LoadIdentity()


        GL.Disable(EnableCap.Lighting)
        GL.Enable(EnableCap.Texture2D)
        GL.Enable(EnableCap.Blend)
        GL.Disable(EnableCap.DepthTest)
        GL.BlendFunc(BlendingFactorSrc.DstColor, BlendingFactorDest.Zero)


        GL.BindTexture(TextureTarget.Texture2D, MyTexture(1))
        GL.Begin(BeginMode.Quads)
        GL.Color3(1.0, 1.0, 1.0)

        GL.TexCoord2(0.0, 1.0)
        GL.Vertex3(-1, -1, -5)

        GL.TexCoord2(0.0, 0.0)
        GL.Vertex3(-1, 1, -5)

        GL.TexCoord2(1.0, 0.0)
        GL.Vertex3(1, 1, -5)

        GL.TexCoord2(1.0, 1.0)
        GL.Vertex3(1, -1, -5)

        GL.End()




        GL.BlendFunc(BlendingFactorSrc.One, BlendingFactorDest.One)
        GL.BindTexture(TextureTarget.Texture2D, MyTexture(0))
        GL.Begin(BeginMode.Quads)
        GL.Color3(1.0, 1.0, 1.0)

        GL.TexCoord2(0.0, 1.0)
        GL.Vertex3(-1, -1, -5)

        GL.TexCoord2(0.0, 0.0)
        GL.Vertex3(-1, 1, -5)

        GL.TexCoord2(1.0, 0.0)
        GL.Vertex3(1, 1, -5)

        GL.TexCoord2(1.0, 1.0)
        GL.Vertex3(1, -1, -5)

        GL.End()








        GL.PopMatrix()
        GL.PopAttrib()

    End Sub

    Protected Sub drawInput(ByVal width As Single, ByVal height As Single, ByVal shiftY As Single)

        GL.PushAttrib(AttribMask.AllAttribBits)
        GL.PushMatrix()
        GL.LoadIdentity()


        GL.Disable(EnableCap.Lighting)
        GL.Enable(EnableCap.Texture2D)
        GL.Enable(EnableCap.Blend)
        GL.Disable(EnableCap.DepthTest)
        GL.BlendFunc(BlendingFactorSrc.DstColor, BlendingFactorDest.Zero)


        GL.BindTexture(TextureTarget.Texture2D, MyTexture(1))
        GL.Begin(BeginMode.Quads)
        GL.Color3(255.0, 255.0, 255.0)

        GL.TexCoord2(0.0, 1.0)
        GL.Vertex3(-width, -height + shiftY, -5)

        GL.TexCoord2(0.0, 0.0)
        GL.Vertex3(-width, height + shiftY, -5)

        GL.TexCoord2(1.0, 0.0)
        GL.Vertex3(width, height + shiftY, -5)

        GL.TexCoord2(1.0, 1.0)
        GL.Vertex3(width, -height + shiftY, -5)

        GL.End()




        GL.BlendFunc(BlendingFactorSrc.One, BlendingFactorDest.One)
        GL.BindTexture(TextureTarget.Texture2D, MyTexture(0))
        GL.Begin(BeginMode.Quads)
        GL.Color3(255.0, 255.0, 255.0)

        GL.TexCoord2(0.0, 1.0)
        GL.Vertex3(-width, -height + shiftY, -5)

        GL.TexCoord2(0.0, 0.0)
        GL.Vertex3(-width, height + shiftY, -5)

        GL.TexCoord2(1.0, 0.0)
        GL.Vertex3(width, height + shiftY, -5)

        GL.TexCoord2(1.0, 1.0)
        GL.Vertex3(width, -height + shiftY, -5)

        GL.End()




        GL.PopMatrix()
        GL.PopAttrib()

    End Sub


    Protected Sub drawInputOrtho1366(ByVal offsetX As Single, ByVal offsetY As Single)

        GL.PushAttrib(AttribMask.AllAttribBits)
        GL.PushMatrix()
        GL.LoadIdentity()


        GL.Disable(EnableCap.Lighting)
        GL.Enable(EnableCap.Texture2D)
        GL.Enable(EnableCap.Blend)
        GL.Disable(EnableCap.DepthTest)
        GL.BlendFunc(BlendingFactorSrc.DstColor, BlendingFactorDest.Zero)


        GL.BindTexture(TextureTarget.Texture2D, MyTexture(1))
        GL.Begin(BeginMode.Quads)
        GL.Color3(1.0, 1.0, 1.0)

        GL.TexCoord2(0.0, 0.0)
        GL.Vertex3(540 + offsetX, 241 + offsetY, -5)

        GL.TexCoord2(0.0, 1.0)
        GL.Vertex3(540 + offsetX, 525 + offsetY, -5)

        GL.TexCoord2(1.0, 1.0)
        GL.Vertex3(824 + offsetX, 525 + offsetY, -5)

        GL.TexCoord2(1.0, 0.0)
        GL.Vertex3(824 + offsetX, 241 + offsetY, -5)

        GL.End()




        GL.BlendFunc(BlendingFactorSrc.One, BlendingFactorDest.One)
        GL.BindTexture(TextureTarget.Texture2D, MyTexture(0))
        GL.Begin(BeginMode.Quads)
        GL.Color3(1.0, 1.0, 1.0)

        GL.TexCoord2(0.0, 0.0)
        GL.Vertex3(540 + offsetX, 241 + offsetY, -5)

        GL.TexCoord2(0.0, 1.0)
        GL.Vertex3(540 + offsetX, 525 + offsetY, -5)

        GL.TexCoord2(1.0, 1.0)
        GL.Vertex3(824 + offsetX, 525 + offsetY, -5)

        GL.TexCoord2(1.0, 0.0)
        GL.Vertex3(824 + offsetX, 241 + offsetY, -5)

        GL.End()








        GL.PopMatrix()
        GL.PopAttrib()

    End Sub



    Protected Sub drawInputOrtho1920(ByVal offsetX As Single, ByVal offsetY As Single)

        GL.PushAttrib(AttribMask.AllAttribBits)
        GL.PushMatrix()
        GL.LoadIdentity()


        GL.Disable(EnableCap.Lighting)
        GL.Enable(EnableCap.Texture2D)
        GL.Enable(EnableCap.Blend)
        GL.Disable(EnableCap.DepthTest)
        GL.BlendFunc(BlendingFactorSrc.DstColor, BlendingFactorDest.Zero)

        GL.BindTexture(TextureTarget.Texture2D, MyTexture(1))
        GL.Begin(BeginMode.Quads)
        GL.Color3(1.0, 1.0, 1.0)

        GL.TexCoord2(0.0, 0.0)
        GL.Vertex3(760 + offsetX, 340 + offsetY, -5)

        GL.TexCoord2(0.0, 1.0)
        GL.Vertex3(760 + offsetX, 740 + offsetY, -5)

        GL.TexCoord2(1.0, 1.0)
        GL.Vertex3(1160 + offsetX, 740 + offsetY, -5)

        GL.TexCoord2(1.0, 0.0)
        GL.Vertex3(1160 + offsetX, 340 + offsetY, -5)

        GL.End()




        GL.BlendFunc(BlendingFactorSrc.One, BlendingFactorDest.One)
        GL.BindTexture(TextureTarget.Texture2D, MyTexture(0))
        GL.Begin(BeginMode.Quads)
        GL.Color3(1.0, 1.0, 1.0)

        GL.TexCoord2(0.0, 0.0)
        GL.Vertex3(760 + offsetX, 340 + offsetY, -5)

        GL.TexCoord2(0.0, 1.0)
        GL.Vertex3(760 + offsetX, 740 + offsetY, -5)

        GL.TexCoord2(1.0, 1.0)
        GL.Vertex3(1160 + offsetX, 740 + offsetY, -5)

        GL.TexCoord2(1.0, 0.0)
        GL.Vertex3(1160 + offsetX, 340 + offsetY, -5)

        GL.End()








        GL.PopMatrix()
        GL.PopAttrib()

    End Sub


    Public Sub disposeTextures()
        If base_NUM_TEXTURES > 0 Then
            GL.DeleteTextures(base_NUM_TEXTURES, base_MyTexture)
        End If
    End Sub

    Public Sub setPrompt(ByVal s As String)
        prompt = s
    End Sub

    Public Sub New(ByVal num_of_textures As Integer, ByVal input_method As gameDevices,
                   ByVal game_screens_type As gameScreens, ByVal duration As Integer,
                   ByVal task_id As Integer, ByVal task_name As String, ByVal takePhoto As Boolean,
                   ByVal op1 As Integer, ByVal op2 As Integer, ByVal op3 As Integer)
        MyBase.New(1920, 1080)
        Try


            currentPrinter = New OpenTK.Graphics.TextPrinter(OpenTK.Graphics.TextQuality.Medium)


            base_NUM_TEXTURES = num_of_textures

            currentInputMethod = input_method
            currentGameScreen = game_screens_type

            currentName = task_name
            currentID = task_id

            currentOption1 = op1
            currentOption2 = op2
            currentOption3 = op3


            reps = 0
            score = 0

            If base_NUM_TEXTURES > 0 Then
                ReDim MyTexture(base_NUM_TEXTURES - 1)
                ReDim textureNames(base_NUM_TEXTURES - 1)
            End If

            initializeNewGameDevices(input_method)


        Catch e As Exception

        End Try

    End Sub



    Protected Sub exitGame()
        If completed = False Then
            completed = True
            Me.Dispose()
        End If
    End Sub

    Protected Sub loadInputTexture(ByVal im As gameDevices)
        Select Case im
            Case gameDevices.trackpad
                textureNames(0) = startUpPath & "\texture\trackpad.jpg"
                textureNames(1) = startUpPath & "\texture\trackpadMask.jpg"
        End Select
    End Sub


    Private Sub updateLoop()
        If Not GlLoaded Then Return

    End Sub

    Private Sub MyGameWindow_UpdateFrame(ByVal sender As Object, ByVal e As OpenTK.FrameEventArgs) Handles Me.UpdateFrame
        updateLoop()
    End Sub

    Private Sub MyGameWindow_RenderFrame(ByVal sender As Object, ByVal e As OpenTK.FrameEventArgs) Handles Me.RenderFrame

        renderLoop()
        OpenTK.Graphics.GraphicsContext.CurrentContext.VSync = True
        SwapBuffers()

    End Sub


    Private Sub MyGameWindow_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

        GlLoaded = True
        GL.ClearColor(Color.Black)

    End Sub

    Private Sub MyGameWindow_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Resize

        setViewPort()

    End Sub

    Protected Overrides Sub Finalize()
        If currentPrinter Is Nothing = False Then currentPrinter.Dispose()
        MyBase.Finalize()
    End Sub
End Class
