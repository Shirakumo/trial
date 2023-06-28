mat3 normal_map_cotangent_frame(in vec3 N, in vec3 p, in vec2 uv){
    // get edge vectors of the pixel triangle
    vec3 dp1 = dFdx(p);
    vec3 dp2 = dFdy(p);
    vec2 duv1 = dFdx(uv);
    vec2 duv2 = dFdy(uv);
 
    // solve the linear system
    vec3 dp2perp = cross(dp2, N);
    vec3 dp1perp = cross(N, dp1);
    vec3 T = dp2perp * duv1.x + dp1perp * duv2.x;
    vec3 B = dp2perp * duv1.y + dp1perp * duv2.y;
 
    // construct a scale-invariant frame 
    float invmax = inversesqrt(max(dot(T,T), dot(B,B)));
    return mat3(T*invmax, B*invmax, N);
}

vec3 normal_map(in sampler2D normal_tex, in vec3 position, in vec2 uv, in vec3 vertex_normal){
  vec3 v_normal = normalize(vertex_normal);
  vec3 normal = texture(normal_tex, uv).rgb * 2.0 - 1.0;
  return normalize(normal_map_cotangent_frame(v_normal, position, uv) * normal);
}
