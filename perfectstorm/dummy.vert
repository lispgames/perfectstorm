varying vec2 TexCoord; 	
void main()
{
        gl_FrontColor = gl_Color;
	gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
	TexCoord = vec2(gl_MultiTexCoord0);
}