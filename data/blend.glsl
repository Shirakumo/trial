#section FRAGMENT_SHADER
out vec4 color;
in vec2 uv;
uniform sampler2D a_pass;
uniform sampler2D b_pass;
uniform int blend_type = 0;

void main(){
  vec4 a = texture(a_pass, uv);
  vec4 b = texture(b_pass, uv);
  
  switch(blend_type){
  case 0: // b-over
    color = mix(a, b, b.a);
    break;
  case 1: // a-over
    color = mix(a, b, a.a);
    break;
  case 2: // Add
    color = a+b;
    break;
  case 3: // Subtract
    color = a-b;
    break;
  case 4: // Multiply
    color = a*b;
    break;
  }
}
