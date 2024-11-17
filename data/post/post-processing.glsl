#section FRAGMENT_SHADER
uniform sampler2D previous_pass;
in vec2 uv;
out vec4 color;

vec4 post_process(sampler2D previous_pass, vec2 uv);

void main(){
  color = post_process(previous_pass, uv);
}
