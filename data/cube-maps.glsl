vec2 cubemap_uv(const vec3 v, out float face){
  vec3 vabs = abs(v);
  float ma = 0.5;
  vec2 uv;
  if(vabs.x <= vabs.z && vabs.y <= vabs.z){
    face = (v.z < 0)? 5 : 4;
    uv = vec2((v.z < 0)? -v.x : v.x, -v.y);
    ma /= vabs.z;
  }else if(vabs.x <= vabs.y){
    face = (v.y < 0)? 3 : 2;
    uv = vec2(v.x, (v.y < 0)? -v.z : v.z);
    ma /= vabs.y;
  }else{
    face = (v.x < 0)? 1 : 0;
    uv = vec2((v.x < 0)? v.z : -v.z, -v.y);
    ma /= vabs.x;
  }
  return uv*ma + 0.5;
}

int cubemap_texture_index(const vec3 v){
  vec3 vabs = abs(v);
  if(vabs.x <= vabs.z && vabs.y <= vabs.z){
    return (v.z < 0)? 5 : 4;
  }else if(vabs.x <= vabs.y){
    return (v.y < 0)? 3 : 2;
  }else{
    return (v.x < 0)? 1 : 0;
  }
}
