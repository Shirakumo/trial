#section FRAGMENT_SHADER
in vec2 uv;
out vec4 color;
uniform sampler2D previous_pass;
uniform vec2 resolution;
uniform float intensity = 3.0;
uniform int down = 1;

void main(){
  vec2 uv = gl_FragCoord.xy / resolution;
  vec2 off = (intensity*0.5) / resolution;
  if(0 < down){
    vec3 sum = texture(previous_pass, uv).rgb * 4.0;
    sum += texture(previous_pass, uv + vec2(-1, -1) * off).rgb;
    sum += texture(previous_pass, uv + vec2(+1, +1) * off).rgb;
    sum += texture(previous_pass, uv + vec2(+1, -1) * off).rgb;
    sum += texture(previous_pass, uv + vec2(-1, +1) * off).rgb;
    color = vec4(sum * 0.125, 1);
  }else{
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
}
