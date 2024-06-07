#section FRAGMENT_SHADER
out vec4 color;
in vec2 uv;
uniform sampler2D t[4];
uniform int channel_count[4] = int[4](4,4,4,4);
uniform int textures_per_line = 1;

void main(){
  // Determine which texture we're currently in.
  int x = int(mod(uv.x*textures_per_line, textures_per_line));
  int y = int(mod(uv.y*textures_per_line, textures_per_line));
  int i = x+y*textures_per_line;

  // Compute texture and local UV
  vec2 uv = vec2(mod(uv.x, textures_per_line)-float(x)/textures_per_line,
                 mod(uv.y, textures_per_line)-float(y)/textures_per_line)
            *textures_per_line;

  // Sample the texture
  vec4 local = vec4(0);
  // Apparently we can't index with a dynamic var...
  switch(i){
  case 0: local = texture(t[0], uv); break;
  case 1: local = texture(t[1], uv); break;
  case 2: local = texture(t[2], uv); break;
  case 3: local = texture(t[3], uv); break;
  }

  int channels = channel_count[i];
  switch(channels){
  case 0: color = vec4(0); break;
  case 1: color = vec4(local.r, local.r, local.r, 1); break;
  case 2: color = vec4(local.rg, 0, 1); break;
  case 3: color = vec4(local.rgb, 1); break;
  case 4: color = local; break;
  }
}
