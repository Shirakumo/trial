# section FRAGMENT_SHADER
uniform sampler2D previous_pass;
uniform sampler2D displacement_map;
uniform float intensity = 1.0;
in vec2 uv;
out vec4 color;

void main(){
  vec2 displacement = texture(displacement_map, uv).rg;
  vec3 previous = texture(previous_pass, uv+displacement*intensity).rgb;
  color = vec4(previous, 1);
}
