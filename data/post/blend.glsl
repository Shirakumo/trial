#section FRAGMENT_SHADER
out vec4 color;
in vec2 uv;
uniform sampler2D a_pass;
uniform sampler2D b_pass;

void main(){
  vec4 a_ = texture(a_pass, uv);
  vec4 b_ = texture(b_pass, uv);
  vec3 a = a_.rgb;
  vec3 b = b_.rgb;
  float a_a = a_.a;
  float b_a = b_.a;
  color.a = mix(a_a, b_a, b_a);
  
  switch(BLEND_TYPE){
  case 0: // b-over
    color = mix(a_, b_, b_a);
    break;
  case 1: // a-over
    color = mix(a_, b_, a_a);
    break;
  case 2: // Add
    color.rgb = a+b;
    break;
  case 3: // Subtract
    color.rgb = a-b;
    break;
  case 4: // Multiply
    color.rgb = a*b;
    break;
  case 5: // Screen
    color.rgb = 1-(1-a)*(1-b);
    break;
  case 6: // Overlay
    color.rgb = vec3((a.r < 0.5)? 2*a.r*b.r : 1-2*(1-a.r)*(1-b.r),
                     (a.g < 0.5)? 2*a.g*b.g : 1-2*(1-a.g)*(1-b.g),
                     (a.b < 0.5)? 2*a.b*b.b : 1-2*(1-a.b)*(1-b.b));
    break;
  case 7: // Hard Light
    color.rgb = vec3((b.r < 0.5)? 2*a.r*b.r : 1-2*(1-a.r)*(1-b.r),
                     (b.g < 0.5)? 2*a.g*b.g : 1-2*(1-a.g)*(1-b.g),
                     (b.b < 0.5)? 2*a.b*b.b : 1-2*(1-a.b)*(1-b.b));
    break;
  case 8: // Soft Light
    color.rgb = (1-2*b)*a*a + 2*b*a;
    break;
  case 9: // Color Dodge
    color.rgb = b/(1-a);
    break;
  case 10: // Color Burn
    color.rgb = 1-((1-b)/a);
    break;
  case 11: // Darken Only
    color.rgb = min(a,b);
    break;
  case 12: // Lighten Only
    color.rgb = max(a,b);
    break;
  case 13: // Divide
    color.rgb = a/b;
    break;
  case 14: // Difference
    color.rgb = vec3((a.r < b.r)? b.r-a.r : a.r-b.r,
                     (a.g < b.g)? b.g-a.g : a.g-b.g,
                     (a.b < b.b)? b.b-a.b : a.b-b.b);
    break;
  case 15: // Linear Burn
    color.rgb = a+b-1;
    break;
  case 16: // Erase
    color.rgb = a;
    color.a = max(0.0,a_a-b_a);
    break;
  }
}
