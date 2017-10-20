Option Explicit On

Imports OpenTK.Graphics.OpenGL

Public Class clsGameDrive
    Inherits clsGame


    Private initialDuration As Single
    Private theta As Single
    Private currentTrackTileID As Single


    Private Structure TrackTile
        'lower left corner
        Dim x1 As Single
        Dim y1 As Single

        'top left corner
        Dim x2 As Single
        Dim y2 As Single

        'top right corner
        Dim x3 As Single
        Dim y3 As Single

        'lower right corner
        Dim x4 As Single
        Dim y4 As Single

    End Structure


    Private Structure Cone
        Dim x1 As Single
        Dim y1 As Single
        Dim x2 As Single
        Dim y2 As Single
        Dim scored As Boolean
        Dim randomD As Single
        Dim greenCone As Boolean
    End Structure


    '**************************************************************************************************
    ' TRACKPOINTS - a constant indicating how many track points in the z direction there are.  Each
    '               track point represents a tile, of which the depth is TRACKDEPTH pixels deep and
    '               the width is represented by the variable TRACKWIDTH.
    '**************************************************************************************************
    Private Const TRACKPOINTS As Integer = 480
    Private Const CONEPOINTS As Integer = TRACKPOINTS / 5

    '**************************************************************************************************
    ' track(TRACKPOINTS) - a 0-based array, initilized to the TRACKPOINTS constant, that is used
    '                      to actually store all the track tiles.
    '**************************************************************************************************
    Private track(TRACKPOINTS) As TrackTile
    Private cones(CONEPOINTS) As Cone

    '**************************************************************************************************
    ' TRACKWIDTH - a constant indicating the width (x direction, left to right on screen)
    '              of each track tile. 
    '**************************************************************************************************
    Private Const TRACKWIDTH As Single = 100

    '**************************************************************************************************
    ' TRACKDEPTH - a constant indicating the depth (z direction, far to near on screen)
    '              of each track tile. 
    '**************************************************************************************************
    Private Const TRACKDEPTH As Single = 100

    '**************************************************************************************************
    ' DtoR - a constant that is used in the conversion of degrees to radians.  All trigonomic 
    '        functions used in the calculations of where the car is needs to be in radians.  
    '**************************************************************************************************
    Private Const DtoR As Single = (Math.PI / 180.0F)

    '**************************************************************************************************
    ' cameraPositionZ - the current position of the camera along the z axis, which is near/far with
    '                   respect to the screen.
    '**************************************************************************************************
    Private cameraPositionZ As Single = 0

    '**************************************************************************************************
    ' cameraPositionX - the current position of the camera along the x axis, which is left/right with
    '                   respect to the screen.
    '**************************************************************************************************
    Private cameraPositionX As Single = 0

    '**************************************************************************************************
    ' cameraRotation - the current rotation of the camera with respect to the origin and rotated about
    '                  the y axis.
    '**************************************************************************************************
    Private cameraRotation As Single = 0
    'Private wheelRotation As Single = 0
    'Public currentMousePosition As Single = 0


    '**************************************************************************************************
    ' speed - the variable that stores the current speed of the car, which is varied depending 
    '         on what terrain is being driven over.
    '**************************************************************************************************
    Private ORIGINALSPEED As Single = 2
    Private speed As Single = ORIGINALSPEED

    '**************************************************************************************************
    ' countDownNumber - this integer is used to indicate the current number texture for the
    '                   textures used during the countdown.  Initialize it to the texture 
    '                   number of Number 3's mask.
    '**************************************************************************************************
    Private countDownNumber As Integer = 8
    Private countDownNumbers() As Boolean = {False, False, False}

    '**************************************************************************************************
    ' countDown - this boolean variable is used to indicate if the game is still in its
    '             initial countdown phase.  If true, display the countdown.  If false, 
    '             start the game and move the car forward.
    '**************************************************************************************************
    Private countDown As Boolean = True

    '**************************************************************************************************
    ' numWidth - this numeric value indicates the size of the numbers that are displayed on the screen
    '            in the timer and the score.  This value represents both the height and the width
    '            of each number.
    '**************************************************************************************************
    Private numWidth As Single = 20


    Private minCone As Single = 30
    Private maxCone As Single = 40



    Private compassRotate As Single = 0
    Private compassIncrementing As Single = 2
    Private coneColor As Single = 0
    Private increasing As Boolean = False
    Private skyRotate As Single = 0

    '**************************************************************************************************
    ' 
    '**************************************************************************************************
    Private Sub decrementCountdown()
        If countDownNumber = 8 Then
            numWidth = 20
            countDownNumber = 10
        ElseIf countDownNumber = 10 Then
            numWidth = 20
            countDownNumber = 12
        ElseIf countDownNumber = 12 Then
            numWidth = 20
            countDownNumber = 14
        Else
            numWidth = 20
            countDown = False
        End If
    End Sub



    Private Sub drawTerrain()
        Dim i As Integer
        Dim j As Integer

        GL.PushAttrib(AttribMask.AllAttribBits)

        GL.Enable(EnableCap.Texture2D)
        GL.BindTexture(TextureTarget.Texture2D, MyTexture(2))

        GL.Begin(BeginMode.Quads)
        GL.Color3(255.0, 255.0, 255.0)


        Dim focalPoint1 As Integer = cameraPositionZ - (cameraPositionZ Mod TRACKWIDTH)
        Dim focalPoint2 As Integer = cameraPositionX - (cameraPositionX Mod TRACKWIDTH)

        For i = -focalPoint1 - 1000 To -focalPoint1 + 1000 Step TRACKWIDTH
            For j = -focalPoint2 - 1000 To -focalPoint2 + 1000 Step TRACKWIDTH
                GL.TexCoord2(0, 0)
                GL.Vertex3(j, -1, i)
                GL.TexCoord2(0, 1)
                GL.Vertex3(j, -1, i + TRACKWIDTH)
                GL.TexCoord2(1, 1)
                GL.Vertex3(j + TRACKWIDTH, -1, i + TRACKWIDTH)
                GL.TexCoord2(1, 0)
                GL.Vertex3(j + TRACKWIDTH, -1, i)

            Next
        Next
        GL.End()

        GL.Disable(EnableCap.Texture2D)

        GL.PopAttrib()

    End Sub




    Private Sub leftWidget(ByVal start As Integer, ByVal length As Integer)

        Dim i As Integer

        Dim turn1 As Integer = (length / 5)
        Dim turn2 As Integer = (length / 5) * 2
        Dim turn3 As Integer = (length / 5) * 3
        Dim turn4 As Integer = (length / 5) * 4
        Dim turn5 As Integer = (length / 5) * 5



        'vertical-up block
        For i = 0 To turn1 - 1
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x3
            track(start + i).y3 = track(start + i - 1).y3 - TRACKDEPTH

            track(start + i).x4 = track(start + i - 1).x4
            track(start + i).y4 = track(start + i - 1).y4 - TRACKDEPTH

        Next

        'up->left block
        For i = turn1 To turn1
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x4
            track(start + i).y3 = track(start + i - 1).y4 - TRACKDEPTH

            track(start + i).x4 = track(start + i - 1).x4
            track(start + i).y4 = track(start + i - 1).y4
        Next

        'horizontal-left block
        For i = turn1 + 1 To turn2 - 1
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i).x2 - TRACKWIDTH
            track(start + i).y3 = track(start + i).y2

            track(start + i).x4 = track(start + i).x1 - TRACKWIDTH
            track(start + i).y4 = track(start + i).y1
        Next

        'left->up block
        For i = turn2 To turn2
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x3
            track(start + i).y3 = track(start + i - 1).y3

            track(start + i).x4 = track(start + i - 1).x3 - TRACKWIDTH
            track(start + i).y4 = track(start + i - 1).y3
        Next

        'vertical-up block
        For i = turn2 + 1 To turn3 - 1
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x3
            track(start + i).y3 = track(start + i - 1).y3 - TRACKDEPTH

            track(start + i).x4 = track(start + i - 1).x4
            track(start + i).y4 = track(start + i - 1).y4 - TRACKDEPTH
        Next

        'up->right block
        For i = turn3 To turn3
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x3
            track(start + i).y3 = track(start + i - 1).y3

            track(start + i).x4 = track(start + i - 1).x3
            track(start + i).y4 = track(start + i - 1).y3 - TRACKDEPTH
        Next

        'horizontal-right block
        For i = turn3 + 1 To turn4 - 1
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i).x2 + TRACKWIDTH
            track(start + i).y3 = track(start + i).y2

            track(start + i).x4 = track(start + i).x1 + TRACKWIDTH
            track(start + i).y4 = track(start + i).y1
        Next

        'right->up block
        For i = turn4 To turn4
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x4 + TRACKWIDTH
            track(start + i).y3 = track(start + i - 1).y4

            track(start + i).x4 = track(start + i - 1).x4
            track(start + i).y4 = track(start + i - 1).y4
        Next

        'vertical-up block
        For i = turn4 + 1 To turn5 - 1
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x3
            track(start + i).y3 = track(start + i - 1).y3 - TRACKDEPTH

            track(start + i).x4 = track(start + i - 1).x4
            track(start + i).y4 = track(start + i - 1).y4 - TRACKDEPTH
        Next
    End Sub



    Private Sub rightWidget(ByVal start As Integer, ByVal length As Integer)

        Dim i As Integer

        Dim turn1 As Integer = (length / 5)
        Dim turn2 As Integer = (length / 5) * 2
        Dim turn3 As Integer = (length / 5) * 3
        Dim turn4 As Integer = (length / 5) * 4
        Dim turn5 As Integer = (length / 5) * 5



        'vertical-up block
        For i = 0 To turn1 - 1
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x3
            track(start + i).y3 = track(start + i - 1).y3 - TRACKDEPTH

            track(start + i).x4 = track(start + i - 1).x4
            track(start + i).y4 = track(start + i - 1).y4 - TRACKDEPTH

        Next

        'up->right block
        For i = turn1 To turn1
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x3
            track(start + i).y3 = track(start + i - 1).y3

            track(start + i).x4 = track(start + i - 1).x3
            track(start + i).y4 = track(start + i - 1).y3 - TRACKDEPTH
        Next

        'horizontal-right block
        For i = turn1 + 1 To turn2 - 1
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i).x2 + TRACKWIDTH
            track(start + i).y3 = track(start + i).y2

            track(start + i).x4 = track(start + i).x1 + TRACKWIDTH
            track(start + i).y4 = track(start + i).y1
        Next

        'right->up block
        For i = turn2 To turn2
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x4 + TRACKWIDTH
            track(start + i).y3 = track(start + i - 1).y4

            track(start + i).x4 = track(start + i - 1).x4
            track(start + i).y4 = track(start + i - 1).y4
        Next

        'vertical-up block
        For i = turn2 + 1 To turn3 - 1
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x3
            track(start + i).y3 = track(start + i - 1).y3 - TRACKDEPTH

            track(start + i).x4 = track(start + i - 1).x4
            track(start + i).y4 = track(start + i - 1).y4 - TRACKDEPTH
        Next

        'up->left block
        For i = turn3 To turn3
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x4
            track(start + i).y3 = track(start + i - 1).y4 - TRACKDEPTH

            track(start + i).x4 = track(start + i - 1).x4
            track(start + i).y4 = track(start + i - 1).y4
        Next

        'horizontal-left block
        For i = turn3 + 1 To turn4 - 1
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i).x2 - TRACKWIDTH
            track(start + i).y3 = track(start + i).y2

            track(start + i).x4 = track(start + i).x1 - TRACKWIDTH
            track(start + i).y4 = track(start + i).y1
        Next

        'left->up block
        For i = turn4 To turn4
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x3
            track(start + i).y3 = track(start + i - 1).y3

            track(start + i).x4 = track(start + i - 1).x3 - TRACKWIDTH
            track(start + i).y4 = track(start + i - 1).y3
        Next

        'vertical-up block
        For i = turn4 + 1 To turn5 - 1
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x3
            track(start + i).y3 = track(start + i - 1).y3 - TRACKDEPTH

            track(start + i).x4 = track(start + i - 1).x4
            track(start + i).y4 = track(start + i - 1).y4 - TRACKDEPTH
        Next






    End Sub


    Private Sub rightZig(ByVal start As Integer, ByVal length As Integer, ByVal increment As Single)

        Dim i As Integer

        Dim turn1 As Integer = (length / 2)
        Dim turn2 As Integer = length

        'zigout
        For i = 0 To turn1 - 1
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x3 + increment
            track(start + i).y3 = track(start + i - 1).y3 - TRACKDEPTH

            track(start + i).x4 = track(start + i - 1).x4 + increment
            track(start + i).y4 = track(start + i - 1).y4 - TRACKDEPTH

        Next

        'zigin
        For i = turn1 To turn2 - 1
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x3 - increment
            track(start + i).y3 = track(start + i - 1).y3 - TRACKDEPTH

            track(start + i).x4 = track(start + i - 1).x4 - increment
            track(start + i).y4 = track(start + i - 1).y4 - TRACKDEPTH
        Next

    End Sub

    Private Sub leftZig(ByVal start As Integer, ByVal length As Integer, ByVal increment As Single)

        Dim i As Integer

        Dim turn1 As Integer = (length / 2)
        Dim turn2 As Integer = length

        'zigout
        For i = 0 To turn1 - 1
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x3 - increment
            track(start + i).y3 = track(start + i - 1).y3 - TRACKDEPTH

            track(start + i).x4 = track(start + i - 1).x4 - increment
            track(start + i).y4 = track(start + i - 1).y4 - TRACKDEPTH

        Next

        'zigin
        For i = turn1 To turn2 - 1

            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x3 + increment
            track(start + i).y3 = track(start + i - 1).y3 - TRACKDEPTH

            track(start + i).x4 = track(start + i - 1).x4 + increment
            track(start + i).y4 = track(start + i - 1).y4 - TRACKDEPTH
        Next

    End Sub


    Private Sub straight(ByVal start As Integer, ByVal length As Integer)

        Dim i As Integer
        For i = 0 To length - 1
            track(start + i).x1 = track(start + i - 1).x4
            track(start + i).y1 = track(start + i - 1).y4

            track(start + i).x2 = track(start + i - 1).x3
            track(start + i).y2 = track(start + i - 1).y3

            track(start + i).x3 = track(start + i - 1).x3
            track(start + i).y3 = track(start + i - 1).y3 - TRACKDEPTH

            track(start + i).x4 = track(start + i - 1).x4
            track(start + i).y4 = track(start + i - 1).y4 - TRACKDEPTH

        Next



    End Sub
    Private Sub initializeTrack()


        'initial block
        track(0).x1 = -(TRACKWIDTH / 2)
        track(0).y1 = 0
        track(0).x2 = (TRACKWIDTH / 2)
        track(0).y2 = 0
        track(0).x3 = (TRACKWIDTH / 2)
        track(0).y3 = -TRACKDEPTH
        track(0).x4 = -(TRACKWIDTH / 2)
        track(0).y4 = -TRACKDEPTH

        straight(1, 5)              '1 + 5 = 6
        leftZig(6, 15, 50)           '6 + 15 = 21
        straight(21, 5)            '21  + 5 = 26
        rightZig(26, 15, 50)        '26 + 15 = 41
        straight(41, 5)             '41 + 5 = 46
        rightWidget(46, 20)         '46 + 20 = 66       'must be divisible by 5
        straight(66, 5)             '66 + 5 = 71
        leftZig(71, 10, 75)         '71+10 = 81
        straight(81, 5)             '81+5 = 86
        rightZig(86, 10, 75)        '86+10 =96
        straight(96, 5)             '96+5 = 101
        leftWidget(101, 20)         '101 + 20 = 121
        straight(121, 5)              '121 + 5 = 126
        leftZig(126, 15, 50)          '126 + 15 = 141
        straight(141, 5)            '141  + 5 = 146
        rightZig(146, 15, 50)        '146 + 15 = 161
        straight(161, 5)             '161 + 5 = 166
        rightWidget(166, 20)         '166 + 20 = 186       'must be divisible by 5
        straight(186, 5)             '186 + 5 = 191
        leftZig(191, 10, 75)         '191+10 = 201
        straight(201, 5)             '201+5 = 206
        rightZig(206, 10, 75)        '206+10 =216
        straight(216, 5)             '216+5 = 221
        leftWidget(221, 20)         '221 + 20 = 241

        straight(241, 5)              '1 + 5 = 6
        leftZig(246, 15, 50)           '6 + 15 = 21
        straight(261, 5)            '21  + 5 = 26
        rightZig(266, 15, 50)        '26 + 15 = 41
        straight(281, 5)             '41 + 5 = 46
        rightWidget(286, 20)         '46 + 20 = 66       'must be divisible by 5
        straight(306, 5)             '66 + 5 = 71
        leftZig(311, 10, 75)         '71+10 = 81
        straight(321, 5)             '81+5 = 86
        rightZig(326, 10, 75)        '86+10 =96
        straight(336, 5)             '96+5 = 101
        leftWidget(341, 20)         '101 + 20 = 121
        straight(361, 5)              '121 + 5 = 126
        leftZig(366, 15, 50)          '126 + 15 = 141
        straight(381, 5)            '141  + 5 = 146
        rightZig(386, 15, 50)        '146 + 15 = 161
        straight(401, 5)             '161 + 5 = 166
        rightWidget(406, 20)         '166 + 20 = 186       'must be divisible by 5
        straight(426, 5)             '186 + 5 = 191
        leftZig(431, 10, 75)         '191+10 = 201
        straight(441, 5)             '201+5 = 206
        rightZig(446, 10, 75)        '206+10 =216
        straight(456, 5)             '216+5 = 221
        leftWidget(461, 20)         '221 + 20 = 241


    End Sub





    Public Sub MoveLeftRight(ByVal angle As Single)
        cameraRotation += angle ' * -90
        If cameraRotation < 0 Then cameraRotation = cameraRotation + 360
        If cameraRotation > 360 Then cameraRotation = cameraRotation - 360
    End Sub


    Private Sub MoveForward()

        Dim x As Single = speed * Math.Sin(-cameraRotation * DtoR)
        Dim y As Single = speed * Math.Cos(-cameraRotation * DtoR)

        cameraPositionZ += y
        cameraPositionX += x


    End Sub







    Private Sub drawCompass()
        Dim X5, Y5 As Single
        Dim X6, Y6 As Single
        Dim H, W As Single




        Dim id As Integer = currentTrackTileID


        If id < 0 Then id = 0
        If id > TRACKPOINTS - 2 Then id = TRACKPOINTS - 2


        X5 = (track(id + 1).x1 + track(id + 1).x2 + track(id + 1).x3 + track(id + 1).x4) / 4
        Y5 = (track(id + 1).y1 + track(id + 1).y2 + track(id + 1).y3 + track(id + 1).y4) / 4

        X6 = (track(id + 2).x1 + track(id + 2).x2 + track(id + 2).x3 + track(id + 2).x4) / 4
        Y6 = (track(id + 2).y1 + track(id + 2).y2 + track(id + 2).y3 + track(id + 2).y4) / 4

        H = Y5 - Y6
        W = X5 - X6

        theta = Math.Round((Math.Atan(W / H) * 180 / Math.PI))
        theta = theta + (theta Mod 2)

        If compassIncrementing < theta Then
            compassIncrementing += 2
        ElseIf compassIncrementing > theta Then
            compassIncrementing -= 2
        End If

        compassRotate = cameraRotation + compassIncrementing

        GL.PushAttrib(AttribMask.AllAttribBits)

        GL.PushMatrix()

        GL.BlendFunc(BlendingFactorSrc.SrcAlpha, BlendingFactorDest.One)

        GL.LoadIdentity()
        GL.Translate(0, 0, -10)
        GL.Rotate(compassRotate, 0, 1, 0)


     

        GL.Translate(0, 5, 0)


        GL.Enable(EnableCap.Blend)
        GL.Color3(0.25, 0, 0)
        GL.Begin(BeginMode.Quads)
        GL.Vertex3(-0.5, 1, 1.0F)
        GL.Vertex3(0.5, 1, 1.0F)
        GL.Vertex3(0.5, 1, -1.0F)
        GL.Vertex3(-0.5, 1, -1.0F)
        GL.End()
        GL.Begin(BeginMode.Triangles)
        GL.Vertex3(1, 1, -1.0F)
        GL.Vertex3(-1, 1, -1.0F)
        GL.Vertex3(0, 1, -3.0F)
        GL.End()
        GL.Begin(BeginMode.LineStrip)
        GL.Vertex3(-0.5, 1, 1.0F)
        GL.Vertex3(0.5, 1, 1.0F)
        GL.Vertex3(0.5, 1, -1.0F)
        GL.Vertex3(1, 1, -1.0F)
        GL.Vertex3(0, 1, -3.0F)
        GL.Vertex3(-1, 1, -1.0F)
        GL.Vertex3(-0.5, 1, -1.0F)
        GL.Vertex3(-0.5, 1, 1.0F)
        GL.End()


        GL.Disable(EnableCap.Blend)
        GL.PopMatrix()
        GL.PopAttrib()

    End Sub





    Private Sub drawNumbers(ByVal num As Char, ByVal scoreWidth As Single)

        Dim x1, x2, y1, y2 As Single

        Select Case num
            Case "0"
                x1 = (0 / 8)
                x2 = x1 + (1 / 8)
                y1 = 0
                y2 = 0.5
            Case "1"
                x1 = (1 / 8)
                x2 = x1 + (1 / 8)
                y1 = 0
                y2 = 0.5
            Case "2"
                x1 = (2 / 8)
                x2 = x1 + (1 / 8)
                y1 = 0
                y2 = 0.5
            Case "3"
                x1 = (3 / 8)
                x2 = x1 + (1 / 8)
                y1 = 0
                y2 = 0.5
            Case "4"
                x1 = (4 / 8)
                x2 = x1 + (1 / 8)
                y1 = 0
                y2 = 0.5
            Case "5"
                x1 = (5 / 8)
                x2 = x1 + (1 / 8)
                y1 = 0
                y2 = 0.5
            Case "6"
                x1 = (6 / 8)
                x2 = x1 + (1 / 8)
                y1 = 0
                y2 = 0.5
            Case "7"
                x1 = (7 / 8)
                x2 = x1 + (1 / 8)
                y1 = 0
                y2 = 0.5
            Case "8"
                x1 = (0 / 8)
                x2 = x1 + (1 / 8)
                y1 = 0.5
                y2 = 1
            Case "9"
                x1 = (1 / 8)
                x2 = x1 + (1 / 8)
                y1 = 0.5
                y2 = 1
            Case "."
                x1 = (2 / 8)
                x2 = x1 + (1 / 8)
                y1 = 0.5
                y2 = 1
            Case Else
                Exit Sub
        End Select



        GL.PushMatrix()



        GL.Enable(EnableCap.Texture2D)
        GL.Disable(EnableCap.Blend)
        GL.Enable(EnableCap.Blend)
        GL.Disable(EnableCap.DepthTest)
        GL.BlendFunc(BlendingFactorSrc.DstColor, BlendingFactorDest.Zero)




        GL.BindTexture(TextureTarget.Texture2D, MyTexture(18))
        GL.Begin(BeginMode.Quads)
        GL.Color3(255.0, 255.0, 255.0)
        GL.TexCoord2(x1, y1)
        GL.Vertex3(-scoreWidth, scoreWidth, -100)
        GL.TexCoord2(x2, y1)
        GL.Vertex3(scoreWidth, scoreWidth, -100)
        GL.TexCoord2(x2, y2)
        GL.Vertex3(scoreWidth, -scoreWidth, -100)
        GL.TexCoord2(x1, y2)
        GL.Vertex3(-scoreWidth, -scoreWidth, -100)
        GL.End()


        GL.BlendFunc(BlendingFactorSrc.One, BlendingFactorDest.One)


        GL.BindTexture(TextureTarget.Texture2D, MyTexture(17))
        GL.Begin(BeginMode.Quads)
        GL.Color3(255.0, 255.0, 255.0)
        GL.TexCoord2(x1, y1)
        GL.Vertex3(-scoreWidth, scoreWidth, -100)
        GL.TexCoord2(x2, y1)
        GL.Vertex3(scoreWidth, scoreWidth, -100)
        GL.TexCoord2(x2, y2)
        GL.Vertex3(scoreWidth, -scoreWidth, -100)
        GL.TexCoord2(x1, y2)
        GL.Vertex3(-scoreWidth, -scoreWidth, -100)
        GL.End()


        GL.Enable(EnableCap.DepthTest)
        GL.Disable(EnableCap.Blend)
        GL.Disable(EnableCap.Texture2D)


        GL.PopMatrix()
    End Sub



    Private Sub drawCountDown()

        GL.PushAttrib(AttribMask.AllAttribBits)

        GL.PushMatrix()
        GL.LoadIdentity()



        GL.Enable(EnableCap.Texture2D)
        GL.Disable(EnableCap.Blend)
        GL.Enable(EnableCap.Blend)
        GL.Disable(EnableCap.DepthTest)
        GL.BlendFunc(BlendingFactorSrc.DstColor, BlendingFactorDest.Zero)

        If numWidth > 0 Then numWidth = numWidth - 0.5



        GL.BindTexture(TextureTarget.Texture2D, MyTexture(countDownNumber))
        GL.Begin(BeginMode.Quads)
        GL.Color3(255.0, 255.0, 255.0)
        GL.TexCoord2(0, 0)
        GL.Vertex3(-numWidth, numWidth, -100)
        GL.TexCoord2(1, 0)
        GL.Vertex3(numWidth, numWidth, -100)
        GL.TexCoord2(1, 1)
        GL.Vertex3(numWidth, -numWidth, -100)
        GL.TexCoord2(0, 1)
        GL.Vertex3(-numWidth, -numWidth, -100)
        GL.End()


        GL.BlendFunc(BlendingFactorSrc.One, BlendingFactorDest.One)


        GL.BindTexture(TextureTarget.Texture2D, MyTexture(countDownNumber - 1))
        GL.Begin(BeginMode.Quads)
        GL.Color3(255.0, 255.0, 255.0)
        GL.TexCoord2(0, 0)
        GL.Vertex3(-numWidth, numWidth, -100)
        GL.TexCoord2(1, 0)
        GL.Vertex3(numWidth, numWidth, -100)
        GL.TexCoord2(1, 1)
        GL.Vertex3(numWidth, -numWidth, -100)
        GL.TexCoord2(0, 1)
        GL.Vertex3(-numWidth, -numWidth, -100)
        GL.End()


        GL.Enable(EnableCap.DepthTest)
        GL.Disable(EnableCap.Blend)
        GL.Disable(EnableCap.Texture2D)


        GL.PopMatrix()
        GL.PopAttrib()
    End Sub



    Private Sub drawDash()

        Static cameraShakeStage As Integer = 0

        Dim dashWidth As Single = 25
        Dim dashHeight As Single = dashWidth / (1366 / 768)

        GL.PushAttrib(AttribMask.AllAttribBits)
        GL.PushMatrix()
        GL.LoadIdentity()

        If isOnTrack() = False Then
            Select Case cameraShakeStage
                Case 0
                    GL.Translate(0, 0, 0)
                    cameraShakeStage = 1
                Case 1
                    GL.Translate(0, -0.25, 0)
                    cameraShakeStage = 2
                Case 2
                    GL.Translate(0, -0.5, 0)
                    cameraShakeStage = 3
                Case 3
                    GL.Translate(0, -0.75, 0)
                    cameraShakeStage = 4
                Case 4
                    GL.Translate(0, -0.5, 0)
                    cameraShakeStage = 5
                Case 5
                    GL.Translate(0, -0.25, 0)
                    cameraShakeStage = 0
            End Select
        Else
            cameraShakeStage = 0
        End If


        GL.Enable(EnableCap.Texture2D)
        GL.Disable(EnableCap.Blend)
        GL.Enable(EnableCap.Blend)
        GL.Disable(EnableCap.DepthTest)
        GL.BlendFunc(BlendingFactorSrc.DstColor, BlendingFactorDest.Zero)

        GL.BindTexture(TextureTarget.Texture2D, MyTexture(5))
        GL.Begin(BeginMode.Quads)
        GL.Color3(255.0, 255.0, 255.0)
        GL.TexCoord2(0, 0.5)
        GL.Vertex3(-dashWidth + 8, dashHeight - 2, -10)
        GL.TexCoord2(1, 0.5)
        GL.Vertex3(dashWidth + 8, dashHeight - 2, -10)
        GL.TexCoord2(1, 1)
        GL.Vertex3(dashWidth + 8, -dashHeight - 2, -10)
        GL.TexCoord2(0, 1)
        GL.Vertex3(-dashWidth + 8, -dashHeight - 2, -10)
        GL.End()

        GL.BlendFunc(BlendingFactorSrc.One, BlendingFactorDest.One)


        GL.BindTexture(TextureTarget.Texture2D, MyTexture(5))
        GL.Begin(BeginMode.Quads)
        GL.Color3(255.0, 255.0, 255.0)
        GL.TexCoord2(0, 0)
        GL.Vertex3(-dashWidth + 8, dashHeight - 2, -10)
        GL.TexCoord2(1, 0)
        GL.Vertex3(dashWidth + 8, dashHeight - 2, -10)
        GL.TexCoord2(1, 0.5)
        GL.Vertex3(dashWidth + 8, -dashHeight - 2, -10)
        GL.TexCoord2(0, 0.5)
        GL.Vertex3(-dashWidth + 8, -dashHeight - 2, -10)
        GL.End()


        GL.Enable(EnableCap.DepthTest)
        GL.Disable(EnableCap.Blend)
        GL.Disable(EnableCap.Texture2D)

        GL.PopMatrix()
        GL.PopAttrib()
    End Sub






    Private Sub initializeCones()

        Dim i As Integer
        Dim j As Integer = 0

        For i = 0 To TRACKPOINTS
            If i Mod 5 = 0 Then
                j = i / 5
                cones(j).randomD = Rand(minCone, maxCone)

                cones(j).x1 = track(i).x1
                cones(j).y1 = track(i).y1

                cones(j).x2 = track(i).x2
                cones(j).y2 = track(i).y2

                If cones(j).y1 = cones(j).y2 Then                   'horizontal, left right axis
                    cones(j).x1 = track(i).x1 + cones(j).randomD
                    cones(j).y1 = track(i).y1
                    cones(j).x2 = track(i).x2 - cones(j).randomD
                    cones(j).y2 = track(i).y2
                Else                                               'vertical, up down axis
                    If cones(j).y1 > cones(j).y2 Then
                        cones(j).x1 = track(i).x1
                        cones(j).y1 = track(i).y1 - cones(j).randomD
                        cones(j).x2 = track(i).x2
                        cones(j).y2 = track(i).y2 + cones(j).randomD
                    Else
                        cones(j).x1 = track(i).x1
                        cones(j).y1 = track(i).y1 + cones(j).randomD
                        cones(j).x2 = track(i).x2
                        cones(j).y2 = track(i).y2 - cones(j).randomD
                    End If
                End If


                cones(j).greenCone = True
                cones(j).scored = False

            End If
        Next
    End Sub



    Private Sub drawRaceTrack()


        GL.PushAttrib(AttribMask.AllAttribBits)
        GL.PushMatrix()
        GL.Disable(EnableCap.DepthTest)
        GL.Disable(EnableCap.Blend)
        GL.Enable(EnableCap.Texture2D)

        Dim i As Integer = 0
        Dim j As Integer
        GL.BindTexture(TextureTarget.Texture2D, MyTexture(3))
        For i = TRACKPOINTS To 0 Step -1
            GL.Begin(BeginMode.Quads)
            GL.Color3(255.0, 255.0, 255.0)
            GL.TexCoord2(0, 0)
            GL.Vertex3(track(i).x1, 0, track(i).y1)
            GL.TexCoord2(1, 0)
            GL.Vertex3(track(i).x2, 0, track(i).y2)
            GL.TexCoord2(1, 1)
            GL.Vertex3(track(i).x3, 0, track(i).y3)
            GL.TexCoord2(0, 1)
            GL.Vertex3(track(i).x4, 0, track(i).y4)
            GL.End()

            If i Mod 5 = 0 Then
                j = i / 5

                If (-cameraPositionZ > cones(j).y1 - 500) And (-cameraPositionZ < cones(j).y1 + 500) Then
                    drawCones(cones(j).x1, cones(j).y1, coneColor, cones(j).greenCone)
                    drawCones(cones(j).x2, cones(j).y2, coneColor, cones(j).greenCone)
                End If


            End If
        Next

        If increasing Then
            coneColor += 1
            If coneColor >= 200 Then
                coneColor = 200
                increasing = False
            End If
        Else
            coneColor -= 1
            If coneColor <= 0 Then
                coneColor = 0
                increasing = True
            End If
        End If



        GL.PopMatrix()
        GL.PopAttrib()
    End Sub


    Private Sub drawCones(ByRef x As Single, ByRef y As Single, ByRef coneColor As Single, ByRef yellowCone As Boolean)
        GL.PushAttrib(AttribMask.AllAttribBits)
        GL.PushMatrix()

        GL.Disable(EnableCap.Lighting)
        GL.Disable(EnableCap.DepthTest)

        If yellowCone Then
            GL.Color3(255.0F, 255.0F, 0)
        Else
            GL.Color3(255.0F, 0, 0)
        End If


        GL.Begin(BeginMode.Quads)
        GL.Vertex3(x - 2.5, 1, y - 2.5)
        GL.Vertex3(x + 2.5, 1, y - 2.5)
        GL.Vertex3(x + 2.5, 1, y + 2.5)
        GL.Vertex3(x - 2.5, 1, y + 2.5)
        GL.End()

        GL.Begin(BeginMode.Triangles)
        GL.Vertex3(x - 2.5, 1, y - 2.5)
        GL.Vertex3(x + 2.5, 1, y - 2.5)
        GL.Vertex3(x, 15, y)
        GL.Vertex3(x + 2.5, 1, y - 2.5)
        GL.Vertex3(x + 2.5, 1, y + 2.5)
        GL.Vertex3(x, 15, y)
        GL.Vertex3(x + 2.5, 1, y + 2.5)
        GL.Vertex3(x - 2.5, 1, y + 2.5)
        GL.Vertex3(x, 15, y)
        GL.Vertex3(x - 2.5, 1, y + 2.5)
        GL.Vertex3(x - 2.5, 1, y - 2.5)
        GL.Vertex3(x, 15, y)
        GL.End()


        GL.LineWidth(0.2)
        GL.Begin(BeginMode.LineStrip)
        GL.Color3(0, 0, 0)
        GL.Vertex3(x - 2.5, 1, y - 2.5)
        GL.Vertex3(x + 2.5, 1, y - 2.5)
        GL.Vertex3(x + 2.5, 1, y + 2.5)
        GL.Vertex3(x - 2.5, 1, y + 2.5)
        GL.End()

        GL.Begin(BeginMode.Lines)
        GL.Vertex3(x - 2.5, 1, y - 2.5)
        GL.Vertex3(x, 15, y)
        GL.Vertex3(x + 2.5, 1, y - 2.5)
        GL.Vertex3(x, 15, y)
        GL.Vertex3(x + 2.5, 1, y + 2.5)
        GL.Vertex3(x, 15, y)
        GL.Vertex3(x - 2.5, 1, y + 2.5)
        GL.Vertex3(x, 15, y)
        GL.End()

        GL.PopMatrix()
        GL.PopAttrib()

    End Sub



    Private Sub drawSphere()


        GL.PushMatrix()
        GL.PushAttrib(AttribMask.AllAttribBits)
        GL.Enable(EnableCap.ColorMaterial)


        GL.Color3(1, 0, 0)
        GL.Translate(0, 0, -10)
        OpenTK.Graphics.Glu.Sphere(quadratic, 0.5, 50, 50)

        GL.PopAttrib()
        GL.PopMatrix()


    End Sub




    Private Function isOnTrack() As Boolean

        'find where on the z track is the camera
        Dim i As Integer
        Dim X_1, X_2, X_3, X_4, Z_1, Z_2, Z_3, Z_4 As Single
        Dim x, y, y0, y1, x0, x1 As Single


        Dim iStart As Integer
        If currentTrackTileID < 0 Then
            iStart = 0
        ElseIf currentTrackTileID > TRACKPOINTS Then
            iStart = TRACKPOINTS
        Else
            iStart = Math.Round(currentTrackTileID)
        End If


        For i = 0 To TRACKPOINTS




            'If -cameraPositionZ >= track(i + 1).Y And -cameraPositionZ <= track(i).Y Then
            X_1 = track(i).x1
            Z_1 = track(i).y1

            X_2 = track(i).x2
            Z_2 = track(i).y2

            X_3 = track(i).x3
            Z_3 = track(i).y3

            X_4 = track(i).x4
            Z_4 = track(i).y4

            'Exit For
            'End If


            '       (4 - x4y4)                         (3 - x3y3)
            '        ___________________________________________
            '       |                     3                     |
            '       |                                           |
            '       |                                           |
            '       |                                           |
            '       |4                                        2 |
            '       |                                           |
            '       |                                           |
            '       |                                           |
            '       |_____________________1_____________________|
            '       (1 - x1y1)                         (2 - x2y2)



            y = -cameraPositionZ
            x = -cameraPositionX

            'calculate which side of the line 1 the camera is on
            x0 = X_1
            y0 = Z_1
            x1 = X_2
            y1 = Z_2
            Dim side1 As Single = (y - y0) * (x1 - x0) - (x - x0) * (y1 - y0)

            'calculate which side of the line 2 the camera is on
            x0 = X_2
            y0 = Z_2
            x1 = X_3
            y1 = Z_3
            Dim side2 As Single = (y - y0) * (x1 - x0) - (x - x0) * (y1 - y0)

            'calculate which side of the line 3 the camera is on
            x0 = X_3
            y0 = Z_3
            x1 = X_4
            y1 = Z_4
            Dim side3 As Single = (y - y0) * (x1 - x0) - (x - x0) * (y1 - y0)

            'calculate which side of the line 4 the camera is on
            x0 = X_4
            y0 = Z_4
            x1 = X_1
            y1 = Z_1
            Dim side4 As Single = (y - y0) * (x1 - x0) - (x - x0) * (y1 - y0)

            'if all sides are of the same magnitude, the the point is on the interior of the polygon
            If side1 <= 0 And side2 <= 0 And side3 <= 0 And side4 <= 0 Then
                isOnTrack = True
                speed = ORIGINALSPEED
                currentTrackTileID = i

                Exit Function
            ElseIf side1 >= 0 And side2 >= 0 And side3 >= 0 And side4 >= 0 Then
                isOnTrack = True
                speed = ORIGINALSPEED
                currentTrackTileID = i

                Exit Function
            End If

        Next

        isOnTrack = False
        'speed = 0.5 * ORIGINALSPEED

        'if the car is 90 to 270, then it's going backwards.  adjust speed accordingly  at 180, it takes 1 second
        ' to decrement.
        '
        'from 270 to 90, the car is going forward. at 0, car is going full speed.


        Dim ss As Single
        If cameraRotation > 180 Then
            ss = (-270 + cameraRotation) / 90
        Else
            ss = (90 - cameraRotation) / 90
        End If
        currentTrackTileID = currentTrackTileID + (ss * 0.03)


    End Function



    Private Sub scored()
        Dim i As Integer
        Dim X_1, X_2, X_3, X_4, Z_1, Z_2, Z_3, Z_4 As Single
        Dim x, y, y0, y1, x0, x1 As Single


        For i = 1 To CONEPOINTS

            If cones(i).scored = False Then

                X_1 = cones(i).x1 - 10
                Z_1 = cones(i).y1 - 10

                X_2 = cones(i).x2 + 10
                Z_2 = cones(i).y2 - 10

                X_3 = cones(i).x2 + 10
                Z_3 = cones(i).y2 + 10

                X_4 = cones(i).x1 - 10
                Z_4 = cones(i).y1 + 10



                '       (4 - x4y4)                         (3 - x3y3)
                '        ___________________________________________
                '       |                     3                     |
                '       |                                           |
                '       |                                           |
                '       |                                           |
                '       |4                                        2 |
                '       |                                           |
                '       |                                           |
                '       |                                           |
                '       |_____________________1_____________________|
                '       (1 - x1y1)                         (2 - x2y2)



                y = -cameraPositionZ
                x = -cameraPositionX

                'calculate which side of the line 1 the camera is on
                x0 = X_1
                y0 = Z_1
                x1 = X_2
                y1 = Z_2
                Dim side1 As Single = (y - y0) * (x1 - x0) - (x - x0) * (y1 - y0)

                'calculate which side of the line 2 the camera is on
                x0 = X_2
                y0 = Z_2
                x1 = X_3
                y1 = Z_3
                Dim side2 As Single = (y - y0) * (x1 - x0) - (x - x0) * (y1 - y0)

                'calculate which side of the line 3 the camera is on
                x0 = X_3
                y0 = Z_3
                x1 = X_4
                y1 = Z_4
                Dim side3 As Single = (y - y0) * (x1 - x0) - (x - x0) * (y1 - y0)

                'calculate which side of the line 4 the camera is on
                x0 = X_4
                y0 = Z_4
                x1 = X_1
                y1 = Z_1
                Dim side4 As Single = (y - y0) * (x1 - x0) - (x - x0) * (y1 - y0)

                'if all sides are of the same magnitude, the the point is on the interior of the polygon
                If side1 <= 0 And side2 <= 0 And side3 <= 0 And side4 <= 0 Then
                    If cones(i).greenCone = True Then
                        cones(i).scored = True
                        score += 1
                    Else
                        cones(i).scored = True
                        If score > 0 Then score -= 1
                    End If
                    Exit Sub
                ElseIf side1 >= 0 And side2 >= 0 And side3 >= 0 And side4 >= 0 Then
                    If cones(i).greenCone = True Then
                        cones(i).scored = True
                        score += 1

                        playSound(startUpPath & "\texture\sounds\sound1.wav")
                    Else
                        cones(i).scored = True
                        If score > 0 Then score -= 1
                    End If
                    Exit Sub
                End If
            End If
        Next

    End Sub



    Private Sub drawSkyCylinder()

        Dim i As Single
        Dim radius As Single = 1000
        Dim degrees As Single = 5
        Dim halfDegrees As Single = degrees / 2
        Dim j As Single = 0
        Dim flip As Boolean = False


        Dim start As Single = halfDegrees
        Dim endS As Single = (halfDegrees + degrees * 5) - degrees


        If skyRotate < 360 Then
            skyRotate = skyRotate + 0.05F
        Else
            skyRotate = 0
        End If



        GL.PushAttrib(AttribMask.AllAttribBits)
        GL.PushMatrix()


        GL.Disable(EnableCap.Lighting)
        GL.Enable(EnableCap.Blend)
        GL.Enable(EnableCap.Texture2D)
        GL.Disable(EnableCap.DepthTest)


        GL.Rotate(skyRotate + cameraRotation, 0, 1, 0)


        GL.BlendFunc(BlendingFactorSrc.DstColor, BlendingFactorDest.Zero)
        GL.Enable(EnableCap.Texture2D)
        GL.BindTexture(TextureTarget.Texture2D, MyTexture(8))
        GL.Begin(BeginMode.QuadStrip)
        GL.Color3(255.0, 255.0, 255.0)
        For i = halfDegrees To halfDegrees + 360 Step degrees

            GL.TexCoord2(j, 0)
            GL.Vertex3(Math.Sin(i * DtoR) * radius, radius, Math.Cos(i * DtoR) * radius)
            GL.TexCoord2(j, 0.5)
            GL.Vertex3(Math.Sin(i * DtoR) * radius, -12, Math.Cos(i * DtoR) * radius)

            j += degrees / 360

        Next
        GL.End()

        GL.BlendFunc(BlendingFactorSrc.One, BlendingFactorDest.One)
        GL.Enable(EnableCap.Texture2D)
        GL.BindTexture(TextureTarget.Texture2D, MyTexture(7))
        GL.Begin(BeginMode.QuadStrip)
        GL.Color4(255.0, 255.0, 255.0, 255.0)
        For i = halfDegrees To halfDegrees + 360 Step degrees

            GL.TexCoord2(j, 0)
            GL.Vertex3(Math.Sin(i * DtoR) * radius, radius, Math.Cos(i * DtoR) * radius)
            GL.TexCoord2(j, 0.5)
            GL.Vertex3(Math.Sin(i * DtoR) * radius, -12, Math.Cos(i * DtoR) * radius)

            j += degrees / 360

        Next
        GL.End()


        GL.Disable(EnableCap.Blend)
        GL.Enable(EnableCap.DepthTest)


        GL.PopMatrix()

        GL.PopAttrib()

    End Sub



    Private Sub drawMountains()

        Dim i As Single
        Dim radius As Single = 1000
        Dim degrees As Single = 5
        Dim halfDegrees As Single = degrees / 2
        Dim j As Single = 0

        GL.PushAttrib(AttribMask.AllAttribBits)
        GL.PushMatrix()

        GL.Disable(EnableCap.Lighting)
        GL.Disable(EnableCap.Blend)
        GL.Enable(EnableCap.Texture2D)
        GL.Enable(EnableCap.DepthTest)

        GL.Rotate(cameraRotation, 0, 1, 0)


        GL.BindTexture(TextureTarget.Texture2D, MyTexture(6))

        GL.Begin(BeginMode.QuadStrip)
        GL.Color3(255.0F, 255.0F, 255.0F)
        For i = halfDegrees To halfDegrees + 360 Step degrees
            GL.TexCoord2(j, 0)
            GL.Vertex3(Math.Sin(i * DtoR) * radius, radius, Math.Cos(i * DtoR) * radius)
            GL.TexCoord2(j, 0.5)
            GL.Vertex3(Math.Sin(i * DtoR) * radius, -12, Math.Cos(i * DtoR) * radius)
            j += degrees / 360
        Next
        GL.End()

        GL.PopMatrix()
        GL.PopAttrib()
    End Sub



    '**************************************************************************************************
    ' This function handles the issue of resizing the OpenGL display with the proper perspective.
    '**************************************************************************************************
    Public Overrides Sub setViewPort()

        '**********************************************************************************************
        ' Set the viewport's size.  Start at the (0,0) coordinate and extend the width and the height
        ' of the display size.
        '**********************************************************************************************
        GL.Viewport(0, 0, SCREENWIDTH, SCREENHEIGHT)

        '**********************************************************************************************
        ' Set the current display matrix as the Projection Matrix.  The Projection Matrix governs 
        ' the perspective effect that is applied to everything that is drawn.  Use this matrix
        ' to initialize the proper views that will be used.
        '**********************************************************************************************
        GL.MatrixMode(MatrixMode.Projection)

        '**********************************************************************************************
        ' Load the identity matrix so that everything is completed with respects to the 
        ' origin point of (0,0,0)
        '**********************************************************************************************
        GL.LoadIdentity()

        '**********************************************************************************************
        ' Set the view perspective.  The OpenGL window will have a horizontal perspective of 85 
        ' degrees.  The width and the height of the screen is used to maintain a proper aspect
        ' ratio.  Finally, set the closest distance in the z plane that can be seen and then set
        ' the furthest distance that be seen in the z plane.
        '**********************************************************************************************
        OpenTK.Graphics.Glu.Perspective(85.0F, SCREENWIDTH / SCREENHEIGHT, 0.1F, 1100.0F)

        '**********************************************************************************************
        ' Set the current display matrix as the Modelview Matrix.  The Modelview Matrix controls 
        ' the position of the camera relative to everything that is drawn.
        '**********************************************************************************************
        GL.MatrixMode(MatrixMode.Modelview)

        '**********************************************************************************************
        ' Load the identity matrix so that everything is completed with respects to the 
        ' origin point of (0,0,0)
        '**********************************************************************************************
        GL.LoadIdentity()
    End Sub
    '***********************************************************************************************************************************

    Public Overrides Sub renderLoop()

        If Not GlLoaded Then Return

        GL.Clear(ClearBufferMask.ColorBufferBit Or ClearBufferMask.DepthBufferBit) '// Clear Screen and Depth Buffer
        GL.LoadIdentity()


        If started = True Then
            If completed = False Then
                inputMethods.getInput(currentInputMethod, reps)


                drawMountains()
                drawSkyCylinder()

                GL.Rotate(cameraRotation, 0, 1, 0)
                GL.Translate(cameraPositionX, -10, cameraPositionZ)


                drawTerrain()
                scored()
                drawRaceTrack()
                drawDash()
                drawCompass()

                If totalTime > (initialDuration - 3) Then
                    If totalTime = (initialDuration - 3) And countDownNumbers(0) = False Then
                        countDownNumbers(0) = True
                        decrementCountdown()
                    ElseIf totalTime = (initialDuration - 2) And countDownNumbers(1) = False Then
                        countDownNumbers(1) = True
                        decrementCountdown()
                    ElseIf totalTime = (initialDuration - 1) And countDownNumbers(2) = False Then
                        countDownNumbers(2) = True
                        decrementCountdown()
                    End If
                    drawCountDown()
                    drawScoreAndTime(score, initialDuration, 200, 0, 0, fontSizes.size_36, 255)
                ElseIf totalTime <= 0 Then
                    exitGame()
                Else
                    Dim carRotation As Single = GlobalCurrentX



                    MoveLeftRight((speed * (carRotation) * 2) - (speed))
                    MoveForward()
                    drawScoreAndTime(score, totalTime, 200, 0, 0, fontSizes.size_36, 255)
                End If

                decrementTimeCheckMyo(currentInputMethod)
            End If
        Else
            drawTextAt(prompt, 255, 255, 255, 255, 20, 20, fontSizes.size_36, fontType.arial)
            drawInput(2, 2, 0.4)
        End If

    End Sub






    Public Overrides Sub handleKeyPress(ByVal sender As Object, ByVal e As OpenTK.KeyPressEventArgs)
        If e.KeyChar = " " Then
            start()
        ElseIf e.KeyChar = ""c Then
            exitGame()
        End If
    End Sub

    Public Overrides Sub handleKeyUp(ByVal k As System.Windows.Forms.Keys)

    End Sub

    Public Overrides Sub initialize()

        Static texturesLoaded As Boolean = False
        If texturesLoaded = False Then
            texturesLoaded = True
            loadTextures()
        End If

        initializeTrack()
        initializeCones()

        compassRotate = 0
        compassIncrementing = 2
        coneColor = 0
        increasing = False
        skyRotate = 0

        theta = 0
        currentTrackTileID = 0

        countDownNumbers(0) = False
        countDownNumbers(1) = False
        countDownNumbers(2) = False

        cameraPositionZ = 0
        cameraPositionX = 0
        cameraRotation = 0


        speed = ORIGINALSPEED
        countDownNumber = 10
        countDown = True
        numWidth = 20
        score = 0
        started = False
        completed = False
    End Sub


    Public Sub New(ByVal taskid1 As Integer, ByVal taskid2 As Integer, ByVal taskname As String, _
                           ByVal duration As Integer, ByVal input_method As gameDevices, _
                           ByVal opt1 As Integer, ByVal opt2 As Integer, ByVal opt3 As Integer, _
                           ByVal game_screen_type As gameScreens, ByVal biman As Boolean, ByVal photo As Boolean)

        MyBase.new(15, input_method, game_screen_type, duration, taskid1, taskname, photo, opt1, opt2, opt3)

        loadInputTexture(input_method)
        textureNames(2) = startUpPath & "\texture\drive\grass.jpg"
        textureNames(3) = startUpPath & "\texture\drive\street.jpg"
        textureNames(4) = startUpPath & "\texture\drive\dashMask.jpg"
        textureNames(5) = startUpPath & "\texture\drive\dash.jpg"
        textureNames(6) = startUpPath & "\texture\drive\mountain.jpg"
        textureNames(7) = startUpPath & "\texture\drive\clouds.jpg"
        textureNames(8) = startUpPath & "\texture\drive\cloudsMask.jpg"
        textureNames(9) = startUpPath & "\texture\drive\CountDown3.jpg"
        textureNames(10) = startUpPath & "\texture\drive\CountDown3Mask.jpg"
        textureNames(11) = startUpPath & "\texture\drive\CountDown2.jpg"
        textureNames(12) = startUpPath & "\texture\drive\CountDown2Mask.jpg"
        textureNames(13) = startUpPath & "\texture\drive\CountDown1.jpg"
        textureNames(14) = startUpPath & "\texture\drive\CountDown1Mask.jpg"




        If opt2 = 3 Then
            minCone = 30
            maxCone = 40
        ElseIf opt2 = 2 Then
            minCone = 20
            maxCone = 30
        Else
            minCone = 10
            maxCone = 20
        End If


        If opt3 = 3 Then
            ORIGINALSPEED = 6
        ElseIf opt3 = 2 Then
            ORIGINALSPEED = 4
        Else
            ORIGINALSPEED = 2
        End If



        MyBase.specifyTextures(textureNames)

        MyBase.setPrompt(moduleGameInstructions.returnPrompt(game_screen_type, duration, input_method))



        totalTime = duration
        initialDuration = duration
    End Sub


End Class


