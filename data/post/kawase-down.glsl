#section FRAGMENT_SHADER
out vec4 color;
uniform sampler2D previous_pass;
uniform float offset = 3.0;

void main(){
  vec2 resolution = vec2(textureSize(previous_pass, 0).xy) * 0.5;
  vec2 uv = gl_FragCoord.xy / resolution;
  vec2 off = offset*0.5 / resolution;
  vec3 sum = texture(previous_pass, uv).rgb * 4.0;
  sum += texture(previous_pass, uv + vec2(-1, -1) * off).rgb;
  sum += texture(previous_pass, uv + vec2(+1, +1) * off).rgb;
  sum += texture(previous_pass, uv + vec2(+1, -1) * off).rgb;
  sum += texture(previous_pass, uv + vec2(-1, +1) * off).rgb;
  color = vec4(sum * 0.125, 1);
}
