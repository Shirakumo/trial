#section FRAGMENT_SHADER
out vec4 color;
uniform sampler2D previous_pass;
uniform float offset = 3.0;

void main(){
  vec2 resolution = vec2(textureSize(previous_pass, 0).xy) * 2.0;
  vec2 uv = gl_FragCoord.xy / resolution;
  vec2 off = offset*0.5 / resolution;
  vec3 sum = texture(previous_pass, uv + vec2(-2, +0) * off).rgb;
  sum += texture(previous_pass, uv + vec2(-1, +1) * off).rgb * 2.0;
  sum += texture(previous_pass, uv + vec2(+0, +2) * off).rgb;
  sum += texture(previous_pass, uv + vec2(+1, +1) * off).rgb * 2.0;
  sum += texture(previous_pass, uv + vec2(+2, +0) * off).rgb;
  sum += texture(previous_pass, uv + vec2(+1, -1) * off).rgb * 2.0;
  sum += texture(previous_pass, uv + vec2(+0, -2) * off).rgb;
  sum += texture(previous_pass, uv + vec2(-1, -1) * off).rgb * 2.0;
  color = vec4(sum / 12.0, 1);
}
