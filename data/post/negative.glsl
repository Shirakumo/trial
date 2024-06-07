#section FRAGMENT_SHADER
in vec2 uv;
out vec4 color;
uniform sampler2D previous_pass;

void main(){
  color = texture(previous_pass, uv);
  color = vec4(1.0-color.x, 1.0-color.y, 1.0-color.z, color.w);
}
