#section FRAGMENT_SHADER
in vec2 uv;
out vec4 color;
uniform sampler2D previous_pass;
uniform vec3 midpoint = vec3(0.5);
uniform vec3 color_filter = vec3(1);
uniform vec3 exposure = vec3(1);
uniform vec3 contrast = vec3(1);
uniform vec3 brightness = vec3(0);
uniform vec3 saturation = vec3(1);
uniform float temperature = 0.0;
uniform float tint = 0.0;
uniform float hue = 0.0;

float luminance(vec3 color) {
  return dot(color, vec3(0.299f, 0.587f, 0.114f));
}

const mat3 LIN_2_LMS_MAT = 
  mat3(3.90405e-1, 5.49941e-1, 8.92632e-3,
       7.08416e-2, 9.63172e-1, 1.35775e-3,
       2.31082e-2, 1.28021e-1, 9.36245e-1);

const mat3 LMS_2_LIN_MAT = 
  mat3(+2.85847e+0, -1.62879e+0, -2.48910e-2,
       -2.10182e-1,  1.15820e+0,  3.24281e-4,
       -4.18120e-2, -1.18169e-1,  1.06867e+0);

vec3 white_balance(vec3 col, float temp, float tint) {
  float t1 = temp * 10.0f / 6.0f;
  float t2 = tint * 10.0f / 6.0f;

  float xf = (t1 < 0) ? 0.1 : 0.05;
  float x = 0.31271 - (t1 * xf);
  float standardIlluminantY = 2.87 * x - 3 * x * x - 0.27509507;
  float y = standardIlluminantY + t2 * 0.05;

  vec3 w1 = vec3(0.949237, 1.03542, 1.08728);

  float Y = 1;
  float X = Y * x / y;
  float Z = Y * (1 - x - y) / y;
  float L = 0.7328 * X + 0.4296 * Y - 0.1624 * Z;
  float M = -0.7036 * X + 1.6975 * Y + 0.0061 * Z;
  float S = 0.0030 * X + 0.0136 * Y + 0.9834 * Z;
  vec3 w2 = vec3(L, M, S);

  vec3 balance = vec3(w1.x / w2.x, w1.y / w2.y, w1.z / w2.z);

  return LMS_2_LIN_MAT*((LIN_2_LMS_MAT*col) * balance);
}

vec3 hue_shift(vec3 col, float shift){
    vec3 P = vec3(0.55735)*dot(vec3(0.55735),col);
    vec3 U = col-P;
    vec3 V = cross(vec3(0.55735),U);    
    return U*cos(shift*6.2832) + V*sin(shift*6.2832) + P;
}

void main(){
  color = texture(previous_pass, uv);
  vec3 col = color.rgb;
  
  col = max(vec3(0), col * exposure);
  col = max(vec3(0), white_balance(col, temperature, tint));
  col = max(vec3(0), contrast * (col - midpoint) + midpoint + brightness);
  col = max(vec3(0), col * color_filter);
  col = max(vec3(0), mix(col, saturation, luminance(col)));
  col = hue_shift(col, hue);

  color.rgb = col;
}
