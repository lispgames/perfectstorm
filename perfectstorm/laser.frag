uniform int TIME_FROM_INIT;
varying vec2 TexCoord;

const float C_PI = 3.1415;
const float C_2PI = (2.0*C_PI);
const float C_2PI_I = (1.0/(2.0*C_PI));
const float C_PI_2 = C_PI / 2.0;
uniform sampler2D sampler2d;
void main (void)
{
    float dist = (0.5-TexCoord.y)*2;
    float factor1 = (0.8 + (0.2 * sin(TexCoord.x * -3.1415 * 2 + (float(TIME_FROM_INIT)/10.0))));
    float factor2 = (1 + (0.1 * sin(TexCoord.x * -3.1415 * 10 + (float(TIME_FROM_INIT)/2.0))));
    float factor3 = (1 + (0.1 * sin(dist*dist * -3.1415 * 6 + (float(TIME_FROM_INIT)/7.0))));
    vec3 lightColor = vec3(texture2D(sampler2d, TexCoord)) * factor2 * factor3 * clamp(1.4-dist,0.0,1.0);
    vec3 ct = clamp(lightColor, 0.0, 1.0);
    gl_FragColor = vec4 (ct , gl_Color.w);
}
