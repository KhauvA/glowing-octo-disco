Public Class Billboard1
    Private Sub Billboard1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

    End Sub

    Private Sub Billboard1_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles Me.Paint
        Dim blackBrush As New Drawing.SolidBrush(Color.Black)
        Dim redBrush As New Drawing.SolidBrush(Color.Red)
        e.Graphics.FillRectangle(blackBrush, 0, 0, 200, 400)
        e.Graphics.FillEcclipse(redBrush, 50, 100, 100, 100)
