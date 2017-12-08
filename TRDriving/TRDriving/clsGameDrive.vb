Option Explicit On

Imports OpenTK.Graphics.OpenGL

Public Class clsGameDrive
    Inherits clsGame

    Private initialDuration As Single
    Private theta As Single
    Private currentTrackTileID As Single

    Private Structure TrackTile
        'lower left corner
        Dim v1 As OpenTK.Vector3

        'top left corner
        Dim v2 As OpenTK.Vector3

        'top right corner
        Dim v3 As OpenTK.Vector3

        'lower right corner
        Dim v4 As OpenTK.Vector3
    End Structure

    Private Structure GameObject
        Dim position As OpenTK.Vector3
    End Structure

    '**************************************************************************************************
    ' TRACKPOINTS - a constant indicating how many track points in the z direction there are.  Each
    '               track point represents a tile, of which the depth is TRACKDEPTH pixels deep and
    '               the width is represented by the variable TRACKWIDTH.
    '**************************************************************************************************
    Private Const TRACKPOINTS As Integer = 480

    '**************************************************************************************************
    ' track(TRACKPOINTS) - a 0-based array, initilized to the TRACKPOINTS constant, that is used
    '                      to actually store all the track tiles.
    '**************************************************************************************************
    Private track(TRACKPOINTS) As TrackTile

    '**************************************************************************************************
    ' TRACKWIDTH - a constant indicating the width (x direction, left to right on screen)
    '              of each track tile. 
    '**************************************************************************************************
    Private Const TRACKWIDTH As Single = 130

    '**************************************************************************************************
    ' TRACKDEPTH - a constant indicating the depth (z direction, far to near on screen)
    '              of each track tile. 
    '**************************************************************************************************
    Private Const TRACKDEPTH As Single = 130

    '**************************************************************************************************
    ' DtoR - a constant that is used in the conversion of degrees to radians.  All trigonomic 
    '        functions used in the calculations of where the car is needs to be in radians.  
    '**************************************************************************************************
    Private Const DtoR As Single = (Math.PI / 180.0F)
    Private Const RtoD As Single = (180.0F / Math.PI)

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

    Private compassRotate As Single = 0
    Private compassIncrementing As Single = 2

    Private skyRotate As Single = 0

    'Custom parameters
    Private Const OBJECTSTART As Integer = 15 ' track tile to start road objects on
    Private Const OBJECTINTERVAL As Integer = 10 ' interval between each road object
    Private Const OBJECTDISTANCE As Integer = 50 ' distance from the track for each side object
    Private Const ZONEOFFSET As Single = 20.0F 'the fuzzy offset of zones, to simulate carwidth detection
    Private Const SHAKEINTENSITY As Single = 0.15F 'height at which shaking when offroad
    Private Const SHAKESPEED As Single = 0.6F 'speed at which shaking occurs
    Private Const TOWCHECK As Integer = 3 'time til towing
    Private Const TOWDURATION As Single = 3.0F 'time to tow
    Private Const DISTRACTIONINTERVAL_MIN As Integer = 2 'minimum interval between distractions
    Private Const DISTRACTIONINVERVAL_MAX As Integer = 3 'maximum interval between distractions
    Private ReadOnly DISTRACTIONDURATION_ARRAY As Integer() = {250, 500, 750, 1000, 1500, 2000, 3000} 'array of hang times for distractions
    Private ReadOnly DISTRACTIONTEXTUREINDEX As Integer() = {20, 22, 24} 'corresponds to mask index in array
    Private Const BUTTONSIZE As Single = 2.25F 'size of distractors
    Private ReadOnly DISTRACTIONCOLORS As OpenTK.Vector3() = { 'colors for distractions
        New OpenTK.Vector3(1, 0, 0),
        New OpenTK.Vector3(0, 1, 0),
        New OpenTK.Vector3(0, 0, 1)}
    Private Const FEEDBACKDURATION As Single = 2.0F 'how long to show feedback
    Private ReadOnly FEEDBACKPOSITIVECOLOR As New OpenTK.Vector3(0, 0.7F, 0)
    Private ReadOnly FEEDBACKNEGATIVECOLOR As New OpenTK.Vector3(1, 0F, 0)
    Private ReadOnly WHITECOLOR As New OpenTK.Vector3(1, 1, 1)
    Private Const SCOREINTERVAL As Double = 1
    Private Const HIGH_SCORE As Integer = 3 'score for staying completely inside during score intervals
    Private Const MED_SCORE As Integer = 2 'score for leaving zone for a split second during an interval
    Private Const LOW_SCORE As Integer = 1 'score for leaving correct side of the road
    Private Const BUMPERSPEED As Single = 0.05F 'speed of camera correction when off track (percentage)

    ' array to store scoring zones
    Private zoneTrack(TRACKPOINTS) As TrackTile
    Private zoneStrict(TRACKPOINTS) As TrackTile
    Private zoneFuzzy(TRACKPOINTS) As TrackTile
    Private objects() As GameObject ' array to store objects

    Private nextDistraction As DateTime 'time til next distraction
    Private endDistraction As DateTime 'time when current distraction ends
    Private currentDistractionShape As Integer = 0 'distractionShape

    Private endFeedback As DateTime  'time to end feedback

    Private nextScore As DateTime = DateTime.Now.AddSeconds(3 + SCOREINTERVAL) 'set initial scoring to after countdown
    Private nextScoreAmount As Integer = HIGH_SCORE 'set next score amount

    Private lastZoneTime As DateTime = DateTime.Now 'time last inside zone

    Private towPosition As OpenTK.Vector3 'towing start position
    Private towTarget As OpenTK.Vector3 'tow target
    Private towStart As DateTime 'tow start time

    'flags
    Private buttonPressed As Boolean = False 'flag to check if button was pressed
    Private wrongButton As Boolean = False 'Flag to check button pressed was wrong
    Private isInZone As Boolean = False 'indicator if car is inside scoring zone
    Private isInHalfZone As Boolean = False 'indicator if car is halfway inside scoring zone
    Private isOnTrack As Boolean = False 'indicator if car is within the track
    Private towing As Boolean = False 'flag is towing
    Private ending As Boolean = False 'flag is ending

    'counters
    Private totalDistractors As Integer = 0
    Private totalCorrectDistractors As Integer = 0
    Private totalIncorrectDistractors As Integer = 0
    Private correctDistractors As Integer = 0
    Private incorrectDistractors As Integer = 0

    'options
    Private map As Long = 0 'map selection
    Private difficulty As Integer = 2 'Difficulty 1 easy, 2 med, 3 hard
    Private bumpers As Boolean = False 'turn bumpers on and off
    Private tow As Boolean = True 'turn on and off towing (doesn't work well with bumpers)
    Private currentDistractionColor As Integer = 0 'distraction

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

#Region "Initalizers"

    Private Sub initializeTrack()
        'initial block
        track(0).v1.X = -(TRACKWIDTH / 2)
        track(0).v1.Z = 0
        track(0).v2.X = (TRACKWIDTH / 2)
        track(0).v2.Z = 0
        track(0).v3.X = (TRACKWIDTH / 2)
        track(0).v3.Z = -TRACKDEPTH
        track(0).v4.X = -(TRACKWIDTH / 2)
        track(0).v4.Z = -TRACKDEPTH

        Dim offset = 0
        Select Case difficulty
            Case 1 'easy
                offset = 0
            Case 2 'medium
                offset = 20
            Case 3 'hard
                offset = 40
        End Select
        For i As Integer = 0 To TRACKPOINTS - 2
            layTrack(i, Rand(-offset, offset))
        Next
    End Sub

    Private Sub layTrack(index As Integer, angle As Single)
        With track(index)
            Dim vector = .v4 - .v3
            Dim magnitude = vector.Length
            Dim normal = OpenTK.Vector3.Normalize(vector)
            Dim origin = .v3 + normal * magnitude / 2.0F

            vector = .v2 - .v1
            normal = OpenTK.Vector3.Normalize(vector)
            Dim mid = .v2 - normal * magnitude / 2.0F

            Dim W = mid.X - origin.X
            Dim H = mid.Z - origin.Z
            Dim R = -Math.Atan2(W, H) * RtoD + angle
            If Math.Abs(R) > 90 Then R = 90 * Math.Sign(R)
            Dim nr = (R - 90) * DtoR
            Dim cr = (R) * DtoR
            Dim n = New OpenTK.Vector3(Math.Cos(nr), 0, Math.Sin(nr)) 'the angle of current track from the middle
            Dim cn = New OpenTK.Vector3(Math.Cos(cr), 0, Math.Sin(cr)) 'the perpendicular vector

            'copy vertexes of the last track
            track(index + 1).v1 = .v4
            track(index + 1).v2 = .v3

            'move up the middle at the angle, and move out perpendicularly for track width
            track(index + 1).v3 = origin + n * TRACKDEPTH + cn * TRACKWIDTH / 2
            track(index + 1).v4 = origin + n * TRACKDEPTH - cn * TRACKWIDTH / 2
        End With
    End Sub

    Private Sub initializeZones()
        For i As Integer = 0 To TRACKPOINTS
            Dim vector = track(i).v2 - track(i).v1
            Dim magnitude = vector.Length
            Dim normal = OpenTK.Vector3.Normalize(vector)

            Dim mp = (magnitude / 2)

            zoneTrack(i).v1 = track(i).v1 - normal * ZONEOFFSET
            zoneTrack(i).v2 = track(i).v2 + normal * ZONEOFFSET

            zoneStrict(i).v1 = track(i).v1 + normal * mp
            zoneStrict(i).v2 = track(i).v2

            zoneFuzzy(i).v1 = track(i).v1 + normal * (mp - ZONEOFFSET)
            zoneFuzzy(i).v2 = track(i).v2 + normal * (ZONEOFFSET)

            vector = track(i).v4 - track(i).v3
            magnitude = vector.Length
            normal = OpenTK.Vector3.Normalize(vector)

            mp = (magnitude / 2)

            zoneTrack(i).v3 = track(i).v3 - normal * ZONEOFFSET
            zoneTrack(i).v4 = track(i).v4 + normal * ZONEOFFSET

            zoneStrict(i).v3 = track(i).v3
            zoneStrict(i).v4 = track(i).v4 - normal * mp

            zoneFuzzy(i).v3 = track(i).v3 - normal * (ZONEOFFSET)
            zoneFuzzy(i).v4 = track(i).v4 - normal * (mp - ZONEOFFSET)
        Next
    End Sub

    Private Sub initializeObjects()
        Dim lst = New List(Of GameObject)

        For i = 0 To TRACKPOINTS
            If i >= OBJECTSTART AndAlso i Mod OBJECTINTERVAL = 0 Then
                Dim n = (track(i).v1 - track(i).v2)
                n.Normalize()

                Dim o As GameObject
                If Rand(0, 1) = 1 Then
                    o.position = OBJECTDISTANCE * n + track(i).v1
                Else
                    o.position = -OBJECTDISTANCE * n + track(i).v2
                End If

                lst.Add(o)
            End If
        Next i
        objects = lst.ToArray()
    End Sub

#End Region

#Region "Drawing Functions"

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

        Select Case map
            Case 0
                GL.BindTexture(TextureTarget.Texture2D, MyTexture(6))
            Case 1
                GL.BindTexture(TextureTarget.Texture2D, MyTexture(18))
        End Select

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

    Private Sub drawTerrain()
        Dim i As Integer
        Dim j As Integer

        GL.PushAttrib(AttribMask.AllAttribBits)

        GL.Enable(EnableCap.Texture2D)

        Select Case map
            Case 0
                GL.BindTexture(TextureTarget.Texture2D, MyTexture(2))
            Case 1
                GL.BindTexture(TextureTarget.Texture2D, MyTexture(17))
        End Select

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
        Static cameraShakeStage As Single = 0
        Static glow As Single = 0

        Dim dashWidth As Single = 25
        Dim dashHeight As Single = dashWidth / (1366 / 768)

        GL.PushAttrib(AttribMask.AllAttribBits)
        GL.PushMatrix()
        GL.LoadIdentity()

        If isOnTrack = False AndAlso Not towing Then
            GL.Translate(0, Math.Sin(cameraShakeStage) * SHAKEINTENSITY, 0)
            cameraShakeStage += SHAKESPEED
        Else
            cameraShakeStage = 0
        End If

        GL.Enable(EnableCap.Texture2D)
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
        GL.Color3(255.0, 255.0, 255.0)
        GL.BindTexture(TextureTarget.Texture2D, MyTexture(5))
        GL.Begin(BeginMode.Quads)
        GL.TexCoord2(0, 0)
        GL.Vertex3(-dashWidth + 8, dashHeight - 2, -10)
        GL.TexCoord2(1, 0)
        GL.Vertex3(dashWidth + 8, dashHeight - 2, -10)
        GL.TexCoord2(1, 0.5)
        GL.Vertex3(dashWidth + 8, -dashHeight - 2, -10)
        GL.TexCoord2(0, 0.5)
        GL.Vertex3(-dashWidth + 8, -dashHeight - 2, -10)
        GL.End()

        Dim w As Single = 0.8F
        Dim h As Single = 0.88F
        Dim x As Single = -0.02F
        Dim y As Single = -0.4F
        GL.BlendFunc(BlendingFactorSrc.DstColor, BlendingFactorDest.Zero)
        GL.BindTexture(TextureTarget.Texture2D, MyTexture(26))
        GL.Color3(255.0, 255.0, 255.0)
        GL.Begin(BeginMode.Quads)
        GL.TexCoord2(0.01, 0.01)
        GL.Vertex3(-w + x, h + y, -10)
        GL.TexCoord2(0.99, 0.01)
        GL.Vertex3(w + x, h + y, -10)
        GL.TexCoord2(0.99, 0.49)
        GL.Vertex3(w + x, -h + y, -10)
        GL.TexCoord2(0.01, 0.49)
        GL.Vertex3(-w + x, -h + y, -10)
        GL.End()

        Dim color = WHITECOLOR
        If buttonPressed Then
            glow += 0.2F
            Dim effect = 0.3F
            If wrongButton Then
                color = FEEDBACKNEGATIVECOLOR
            Else
                color = FEEDBACKPOSITIVECOLOR
            End If
            color += Math.Sin(glow) * effect * New OpenTK.Vector3(1, 1, 1)

            If endFeedback < Now Then
                buttonPressed = False
                wrongButton = False
            End If
        Else
            glow = 0
        End If

        GL.BlendFunc(BlendingFactorSrc.One, BlendingFactorDest.One)
        GL.BindTexture(TextureTarget.Texture2D, MyTexture(26))
        GL.Color3(color)
        GL.Begin(BeginMode.Quads)
        GL.TexCoord2(0.01, 0.51)
        GL.Vertex3(-w + x, h + y, -10)
        GL.TexCoord2(0.99, 0.51)
        GL.Vertex3(w + x, h + y, -10)
        GL.TexCoord2(0.99, 0.99)
        GL.Vertex3(w + x, -h + y, -10)
        GL.TexCoord2(0.01, 0.99)
        GL.Vertex3(-w + x, -h + y, -10)
        GL.End()

        If nextDistraction < Now AndAlso Now < endDistraction Then drawButton()

        GL.Enable(EnableCap.DepthTest)
        GL.Disable(EnableCap.Blend)
        GL.Disable(EnableCap.Texture2D)

        GL.PopMatrix()
        GL.PopAttrib()
    End Sub

    Private Sub drawRaceTrack()
        GL.PushAttrib(AttribMask.AllAttribBits)
        GL.PushMatrix()
        GL.Disable(EnableCap.DepthTest)
        GL.Disable(EnableCap.Blend)
        GL.Enable(EnableCap.Texture2D)

        Dim i As Integer = 0
        GL.BindTexture(TextureTarget.Texture2D, MyTexture(If(bumpers, 25, 3)))
        GL.Begin(BeginMode.Quads)
        For i = TRACKPOINTS To 0 Step -1
            GL.Color3(255.0, 255.0, 255.0)
            GL.TexCoord2(0, 0)
            GL.Vertex3(track(i).v1)
            GL.TexCoord2(1, 0)
            GL.Vertex3(track(i).v2)
            GL.TexCoord2(1, 1)
            GL.Vertex3(track(i).v3)
            GL.TexCoord2(0, 1)
            GL.Vertex3(track(i).v4)
        Next
        GL.End()

        For Each obj In objects
            drawObject(obj)
        Next

        GL.PopMatrix()
        GL.PopAttrib()
    End Sub

    Private Sub drawButton()
        Dim x As Single = 0F
        Dim y As Single = 6.75F
        Dim w As Single = BUTTONSIZE

        GL.BlendFunc(BlendingFactorSrc.DstColor, BlendingFactorDest.Zero)
        GL.Color3(255.0F, 255.0F, 255.0F)
        GL.BindTexture(TextureTarget.Texture2D, MyTexture(DISTRACTIONTEXTUREINDEX(currentDistractionShape)))
        GL.Begin(BeginMode.Quads)
        GL.TexCoord2(0, 0)
        GL.Vertex3(-w + x, w + y, -10)
        GL.TexCoord2(1, 0)
        GL.Vertex3(w + x, w + y, -10)
        GL.TexCoord2(1, 1)
        GL.Vertex3(w + x, -w + y, -10)
        GL.TexCoord2(0, 1)
        GL.Vertex3(-w + x, -w + y, -10)
        GL.End()

        GL.BlendFunc(BlendingFactorSrc.One, BlendingFactorDest.One)

        GL.BindTexture(TextureTarget.Texture2D, MyTexture(DISTRACTIONTEXTUREINDEX(currentDistractionShape) - 1))
        GL.Color3(DISTRACTIONCOLORS(currentDistractionColor))
        GL.Begin(BeginMode.Quads)
        GL.TexCoord2(0, 0)
        GL.Vertex3(-w + x, w + y, -10)
        GL.TexCoord2(1, 0)
        GL.Vertex3(w + x, w + y, -10)
        GL.TexCoord2(1, 1)
        GL.Vertex3(w + x, -w + y, -10)
        GL.TexCoord2(0, 1)
        GL.Vertex3(-w + x, -w + y, -10)
        GL.End()
    End Sub

    Private Sub drawObject(obj As GameObject)
        Dim size As Single = 50

        GL.PushAttrib(AttribMask.AllAttribBits)
        GL.PushMatrix()

        Dim r = Math.Atan2(-cameraPositionX - obj.position.X, -cameraPositionZ - obj.position.Z)

        GL.Translate(obj.position)
        GL.Rotate(r * RtoD, 0, 1, 0)

        GL.Enable(EnableCap.Texture2D)
        GL.Disable(EnableCap.Blend)
        GL.Enable(EnableCap.Blend)
        GL.Disable(EnableCap.DepthTest)
        GL.BlendFunc(BlendingFactorSrc.DstColor, BlendingFactorDest.Zero)

        GL.Color3(255.0F, 255.0F, 255.0F)
        GL.BindTexture(TextureTarget.Texture2D, MyTexture(16))
        GL.Begin(BeginMode.Quads)
        GL.TexCoord2(0, 1)
        GL.Vertex3(size, 0, 0)
        GL.TexCoord2(1, 1)
        GL.Vertex3(-size, 0, 0)
        GL.TexCoord2(1, 0)
        GL.Vertex3(-size, 2 * size, 0)
        GL.TexCoord2(0, 0)
        GL.Vertex3(size, 2 * size, 0)
        GL.End()

        GL.BlendFunc(BlendingFactorSrc.One, BlendingFactorDest.One)

        GL.Color3(255.0F, 255.0F, 255.0F)
        GL.BindTexture(TextureTarget.Texture2D, MyTexture(15))
        GL.Begin(BeginMode.Quads)
        GL.TexCoord2(0, 1)
        GL.Vertex3(size, 0, 0)
        GL.TexCoord2(1, 1)
        GL.Vertex3(-size, 0, 0)
        GL.TexCoord2(1, 0)
        GL.Vertex3(-size, 2 * size, 0)
        GL.TexCoord2(0, 0)
        GL.Vertex3(size, 2 * size, 0)
        GL.End()

        GL.Enable(EnableCap.DepthTest)
        GL.Disable(EnableCap.Blend)
        GL.Disable(EnableCap.Texture2D)

        GL.PopMatrix()
        GL.PopAttrib()
    End Sub

    Private Sub drawDashboard()
        drawTextAt("Total Distractors: " & totalDistractors, 255, 255, 255, 255, 50, 100, fontSizes.size_36, fontType.arial)
        drawTextAt("Total Correct Distractors: " & totalCorrectDistractors, 255, 255, 255, 255, 50, 140, fontSizes.size_36, fontType.arial)
        drawTextAt("Total Incorrect Distractors: " & totalIncorrectDistractors, 255, 255, 255, 255, 50, 180, fontSizes.size_36, fontType.arial)
        drawTextAt("Correct Press: " & correctDistractors, 255, 255, 255, 255, 50, 220, fontSizes.size_36, fontType.arial)
        drawTextAt("Incorrect Press: " & incorrectDistractors, 255, 255, 255, 255, 50, 260, fontSizes.size_36, fontType.arial)
        drawTextAt("Correct Percent: " & (correctDistractors / totalCorrectDistractors).ToString("p"), 255, 255, 255, 255, 50, 300, fontSizes.size_36, fontType.arial)
        drawTextAt("Inorrect Percent: " & (incorrectDistractors / totalIncorrectDistractors).ToString("p"), 255, 255, 255, 255, 50, 340, fontSizes.size_36, fontType.arial)
    End Sub
#End Region

#Region "Game state checking"

    Private Function calcSide(x As Single, y As Single, v0 As OpenTK.Vector3, v1 As OpenTK.Vector3) As Single
        Return (y - v0.Z) * (v1.X - v0.X) - (x - v0.X) * (v1.Z - v0.Z)
    End Function

    Private Function isPointInQuad(x As Single, y As Single, tile As TrackTile) As Boolean
        'calculate which side of the line 1 the camera is on
        Dim side1 As Single = calcSide(x, y, tile.v1, tile.v2)

        'calculate which side of the line 2 the camera is on
        Dim side2 As Single = calcSide(x, y, tile.v2, tile.v3)

        'calculate which side of the line 3 the camera is on
        Dim side3 As Single = calcSide(x, y, tile.v3, tile.v4)

        'calculate which side of the line 4 the camera is on
        Dim side4 As Single = calcSide(x, y, tile.v4, tile.v1)

        'if all sides are of the same magnitude, the the point is on the interior of the polygon
        If (side1 <= 0 AndAlso side2 <= 0 AndAlso side3 <= 0 AndAlso side4 <= 0) OrElse (side1 >= 0 AndAlso side2 >= 0 AndAlso side3 >= 0 AndAlso side4 >= 0) Then
            Return True
        End If
        Return False
    End Function

    Private Sub checkPosition() ' replaced isOnTrack function
        'find where on the z track is the camera
        Dim i As Integer

        Dim iStart As Integer
        If currentTrackTileID < 0 Then
            iStart = 0
        ElseIf currentTrackTileID > TRACKPOINTS Then
            iStart = TRACKPOINTS
        Else
            iStart = Math.Round(currentTrackTileID)
        End If

        isInZone = False
        isOnTrack = False
        isInHalfZone = False

        For i = 0 To TRACKPOINTS
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
            If isPointInQuad(-cameraPositionX, -cameraPositionZ, zoneTrack(i)) Then
                isOnTrack = True
                lastZoneTime = DateTime.Now
                speed = ORIGINALSPEED
                currentTrackTileID = i

                If isPointInQuad(-cameraPositionX, -cameraPositionZ, zoneStrict(i)) Then
                    isInZone = True
                ElseIf isPointInQuad(-cameraPositionX, -cameraPositionZ, zoneFuzzy(i)) Then
                    isInHalfZone = True
                    nextScoreAmount = MED_SCORE
                Else
                    nextScoreAmount = LOW_SCORE
                End If

                Exit Sub
            End If
        Next

        nextScoreAmount = 0
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
        'currentTrackTileID = currentTrackTileID + (ss * 0.03)
    End Sub

    Private Sub checkScoring()
        If nextScore < DateTime.Now Then 'if current time to check score has passed
            score += nextScoreAmount 'add next score

            nextScoreAmount = HIGH_SCORE 'reset next score
            'set time to check score to 1 second from now
            nextScore = DateTime.Now.AddSeconds(SCOREINTERVAL)
        End If
    End Sub

    Private Sub checkDistractions()
        If endDistraction < DateTime.Now AndAlso nextDistraction < DateTime.Now Then 'else if current time is pass the end of a distraction
            If endDistraction < nextDistraction Then  'else if current time is pass the end of a distraction
                setEndDistraction()
                totalDistractors += 1

                'set current distraction to random distraction
                If Rand(0, 100) < 50 Then
                    totalCorrectDistractors += 1
                    currentDistractionShape = 0
                Else
                    totalIncorrectDistractors += 1
                    currentDistractionShape = Math.Min(2, Rand(1, 3))
                End If
            Else 'if current time has passed time for next distraction
                setNextDistraction()
            End If
        End If
    End Sub

    Private Sub checkOffTrack()
        With track(currentTrackTileID)
            'get midpoint of next track
            Dim mp = ((.v1 + .v2 + .v3 + .v4) / 4)

            'find rotation between camera and next track
            Dim W = -cameraPositionX - mp.X
            Dim H = -cameraPositionZ - mp.Z
            Dim R = -Math.Round(Math.Atan2(W, H) * 180 / Math.PI)

            'line with track
            'Dim W = .x1 - .x4
            'Dim H = .y1 - .y4
            'Dim R = -Math.Round(Math.Atan2(W, H) * 180 / Math.PI)

            If R < 0 Then R += 360
            If R > 360 Then R -= 360

            'find difference between current rotation and target rotation
            Dim d = R - cameraRotation
            If d < 0 Then d += 360
            If d > 360 Then d -= 360
            If d > 180 Then d -= 360

            'if car is not on track, rotate to target
            If Not isOnTrack AndAlso Math.Abs(d) > 1 AndAlso bumpers Then cameraRotation += Math.Max(Math.Abs(d) * BUMPERSPEED, 1) * Math.Sign(d)
        End With
    End Sub

    Private Sub checkTowTruck()
        If (DateTime.Now - lastZoneTime).TotalSeconds > TOWCHECK Then 'if its been TOWCHECK amount of seconds since the last time it was in a zone
            towing = True

            endDistraction = Now
            setNextDistraction(TOWDURATION)

            towStart = DateTime.Now
            towPosition.X = cameraPositionX 'set start position to current camera position
            towPosition.Y = 0
            towPosition.Z = cameraPositionZ

            With track(currentTrackTileID) 'set target position to 25% of track width of current track
                Dim v = .v1 - .v2
                Dim m = v.Length
                v.Normalize()

                towTarget = .v2 + v * m * 0.25F

                v = .v2 - .v3
                v.Normalize()
                Dim a = -Math.Atan2(v.X, v.Z) * RtoD

                towPosition.Y = cameraRotation
                towTarget *= -1

                If Math.Abs(cameraRotation - (a + 360)) < Math.Abs(cameraRotation - a) Then 'choose the smallest angle to rotate from
                    towTarget.Y = a + 360
                Else
                    towTarget.Y = a
                End If
            End With
        End If
    End Sub

    Private Sub scoreDistraction()
        If totalTime <= (initialDuration - 3) AndAlso Not buttonPressed Then
            buttonPressed = True
            endFeedback = Now.AddSeconds(FEEDBACKDURATION)

            If nextDistraction < Now AndAlso Now < endDistraction Then
                endDistraction = endFeedback
                If currentDistractionShape = 0 Then
                    correctDistractors += 1
                Else
                    incorrectDistractors += 1
                    wrongButton = True
                End If
            Else
                wrongButton = True
            End If
        End If
    End Sub
#End Region

#Region "Movement"

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

#End Region

    Private Sub setNextDistraction(Optional offset As Integer = 0)
        'set time to end distraction and set new time for next distraction
        nextDistraction = DateTime.Now.AddSeconds(offset + Rand(DISTRACTIONINTERVAL_MIN, DISTRACTIONINVERVAL_MAX))
    End Sub
    Private Sub setEndDistraction()
        endDistraction = DateTime.Now.AddMilliseconds(DISTRACTIONDURATION_ARRAY(Rand(0, DISTRACTIONDURATION_ARRAY.Length - 1)))
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
                If ending = False Then
                    inputMethods.getInput(currentInputMethod, reps)
                    checkPosition()
                    checkDistractions()
                    checkOffTrack()

                    drawMountains()
                    drawSkyCylinder()

                    GL.Rotate(cameraRotation, 0, 1, 0)
                    GL.Translate(cameraPositionX, -10, cameraPositionZ)

                    drawTerrain()
                    drawRaceTrack()
                    drawDash()

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
                        ending = True
                    ElseIf towing Then 'if towing
                        Dim elapsed = (DateTime.Now - towStart).TotalSeconds 'get how long its been towing
                        Dim n = OpenTK.Vector3.Lerp(towPosition, towTarget, Math.Min(1.0F, elapsed / TOWDURATION)) 'linearly interpolate between start positiong and target

                        cameraPositionX = n.X 'move camera
                        cameraPositionZ = n.Z
                        cameraRotation = n.Y

                        'if duartion finished stop towing
                        If elapsed > TOWDURATION Then
                            towing = False
                            lastZoneTime = DateTime.Now
                            nextScore = DateTime.Now.AddSeconds(SCOREINTERVAL)
                        End If
                        drawScoreAndTime(score, totalTime, 200, 0, 0, fontSizes.size_36, 255)
                    Else
                        Dim carRotation As Single = GlobalCurrentX
                        checkTowTruck()

                        MoveLeftRight((speed * (carRotation) * 2) - (speed))
                        MoveForward()
                        checkScoring()
                        drawScoreAndTime(score, totalTime, 200, 0, 0, fontSizes.size_36, 255)
                    End If

                    'debugging messages
                    decrementTimeCheckMyo(currentInputMethod)
                Else
                    drawDashboard()
                End If
            End If
        Else
            drawTextAt(prompt, 255, 255, 255, 255, 20, 20, fontSizes.size_36, fontType.arial)
            drawInput(2, 2, 0.4)
        End If
    End Sub

    Public Overrides Sub handleKeyPress(ByVal sender As Object, ByVal e As OpenTK.KeyPressEventArgs)
        If e.KeyChar = " " Then
            start()
            scoreDistraction()
            If ending Then exitGame()
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
        initializeZones()
        initializeObjects()

        compassRotate = 0
        compassIncrementing = 2

        skyRotate = 0

        theta = 0
        currentTrackTileID = 0

        countDownNumbers(0) = False
        countDownNumbers(1) = False
        countDownNumbers(2) = False

        cameraPositionZ = 0
        cameraPositionX = -25
        cameraRotation = 0

        speed = ORIGINALSPEED
        countDownNumber = 10
        countDown = True
        numWidth = 20
        score = 0
        started = False
        completed = False

        'TODO: set options here!
        map = 0
        currentDistractionColor = (Rand(0, 100) Mod DISTRACTIONCOLORS.Length)
        bumpers = False
        tow = True

        'set an endtime for distractions in the past so game doesnt show distraction right away
        endDistraction = DateTime.Now.AddSeconds(-1)
        setNextDistraction(3)
    End Sub

    Public Sub New(ByVal taskid1 As Integer, ByVal taskid2 As Integer, ByVal taskname As String,
                           ByVal duration As Integer, ByVal input_method As gameDevices,
                           ByVal opt1 As Integer, ByVal opt2 As Integer, ByVal opt3 As Integer,
                           ByVal game_screen_type As gameScreens, ByVal biman As Boolean, ByVal photo As Boolean)

        MyBase.New(27, input_method, game_screen_type, duration, taskid1, taskname, photo, opt1, opt2, opt3)

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
        textureNames(15) = startUpPath & "\texture\drive\whiteBillboard.jpg"
        textureNames(16) = startUpPath & "\texture\drive\maskBillboard.jpg"
        textureNames(17) = startUpPath & "\texture\drive\sand.jpg"
        textureNames(18) = startUpPath & "\texture\drive\beach.jpg"
        textureNames(19) = startUpPath & "\texture\drive\circle.jpg"
        textureNames(20) = startUpPath & "\texture\drive\circle-mask.jpg"
        textureNames(21) = startUpPath & "\texture\drive\square.jpg"
        textureNames(22) = startUpPath & "\texture\drive\square-mask.jpg"
        textureNames(23) = startUpPath & "\texture\drive\triangle.jpg"
        textureNames(24) = startUpPath & "\texture\drive\triangle-mask.jpg"
        textureNames(25) = startUpPath & "\texture\drive\street-bumper.jpg"
        textureNames(26) = startUpPath & "\texture\drive\indicator.jpg"

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