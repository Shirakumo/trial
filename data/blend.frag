out vec4 color;
in vec2 tex_coord;
uniform sampler2D a_pass;
uniform sampler2D b_pass;
uniform int blend_type = 0;

void main(){
  vec4 a = texture(a_pass, tex_coord);
  vec4 b = texture(b_pass, tex_coord);
  
  switch(blend_type){
  case 0: // Add
    color = a+b;
    break;
  case 1: // Subtract
    color = a-b;
    break;
  case 2: // Multiply
    color = a*b;
    break;
  case 3: // 
    break;
  case 4: // 
    break;
  case 5: // 
    break;
  case 6: // 
    break;
  case 7: // 
    break;
  case 8: // 
    break;
  }
}
