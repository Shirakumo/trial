void main(){
    mat4 r = gl_ModelViewMatrix;
    r[3][0] = 0.0;
    r[3][1] = 0.0;
    r[3][2] = 0.0;

    vec4 v = inverse(r) * inverse(gl_ProjectionMatrix) * gl_Vertex;

    gl_TexCoord[0] = v; 
    gl_Position    = gl_Vertex;
}
