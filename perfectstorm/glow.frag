uniform int TIME_FROM_INIT;
varying vec2 TexCoord;

const float C_PI = 3.1415;
const float C_2PI = (2.0*C_PI);
const float C_2PI_I = (1.0/(2.0*C_PI));
const float C_PI_2 = C_PI / 2.0;
uniform sampler2D sampler2d;
void main (void)
{
    float x = TexCoord.x - 0.5;
    float y = TexCoord.y - 0.5;
    float rot = atan(x,y) * 18.0 + TIME_FROM_INIT/2.0;
    float dist = clamp(sqrt(x*x + y*y) * 2.0, 0.0, 1.0);
    float corona = 1.0 + 0.2 * (0 + sin(rot)) * dist;
    float pulse =  1.0 + dist * 0.2 * (1.0 - exp2(sin(dist*10.0 + rot/6.0 - TIME_FROM_INIT/8.0)));
    vec4 lightColor = vec4(texture2D(sampler2d, TexCoord)) * corona * pulse;
    vec4 ct = clamp(lightColor, 0.0, 1.0);
    float alpha = gl_Color.w +  pow(1.0-dist,4.0)*0.2;
    alpha = ct.w * clamp(alpha, 0.0, 1.0);
    gl_FragColor = vec4(ct.x, ct.y, ct.z ,alpha);
}