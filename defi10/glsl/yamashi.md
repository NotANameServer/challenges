Pour le fun je me suis dit que j'allais tout faire sur le GPU, donc voila le shader toy: https://www.shadertoy.com/view/fdK3zd

# Common

```glsl
const int sphereCount                = 30;
const ivec2 txSpheres                = ivec2(0, 0);
const ivec2 txSphereVelocities       = ivec2(1, 0);
const ivec2 txSphereColors           = ivec2(2, 0);
```

# Buffer A

Mettre `Buffer A` dans `iChannel0`.

```glsl
vec4 loadValue( in ivec2 re )
{
    return texelFetch( iChannel0, re, 0 );
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    ivec2 ifragCoord = ivec2( fragCoord-0.5 );

    if( ifragCoord.x > 2 || ifragCoord.y > sphereCount) discard;
   
    float frame_delta = min(iTimeDelta, 0.05);

    int sphereIdx = ifragCoord.y;
    
    int xDiv = int(iResolution.x / (40.0 + float(sphereCount * 3)));
    int yDiv = int(iResolution.y / (40.0 + float(sphereCount * 3)));
    
    float offsetx = float(sphereIdx % xDiv) * (float(iResolution.x) / float(xDiv));
    float offsety = float(sphereIdx / xDiv) * (float(iResolution.y) / float(yDiv));

    vec4 sphereDef = vec4(30.0 + offsetx, 30.0 + offsety , 15.0 + float(sphereIdx), 20.0 + float(sphereIdx));
    vec4 velocity = vec4(100.0, 200.0, 0.0, 0.0);

    if(iFrame > 0)
    {
        sphereDef = loadValue(txSpheres + ivec2(0, sphereIdx));
        velocity = loadValue(txSpheres + ivec2(1, sphereIdx));
    }

    vec4 futureDef = sphereDef + vec4(velocity.xy * frame_delta, 0, 0);

    float had_colided = velocity.z;
    float colided = 0.0;
    
    vec4 bounds = vec4(iResolution.x, iResolution.y, 0.0, 0.0);
    vec4 component_bounds = vec4(futureDef.x + futureDef.z, futureDef.y + futureDef.z, futureDef.x - futureDef.z, futureDef.y - futureDef.z);

    bvec2 upper_bound_limits = greaterThan(component_bounds.xy, bounds.xy);
    bvec2 lower_bound_limits = lessThan(component_bounds.zw, bounds.zw);
    
    // Test bounds
    velocity.x = mix(velocity.x, (velocity.x > 0.0 ? -velocity.x : velocity.x), upper_bound_limits.x);
    velocity.x = mix(velocity.x, (velocity.x < 0.0 ? -velocity.x : velocity.x), lower_bound_limits.x);
    velocity.y = mix(velocity.y, (velocity.y > 0.0 ? -velocity.y : velocity.y), upper_bound_limits.y);
    velocity.y = mix(velocity.y, (velocity.y < 0.0 ? -velocity.y : velocity.y), lower_bound_limits.y);
    
    for(int i = 0; i < sphereCount; ++i)
    {
        if(i == sphereIdx)
            continue;

        vec4 otherSphereDef = loadValue(txSpheres + ivec2(0, i));
        vec4 otherVelocity = loadValue(txSpheres + ivec2(1, i));

        vec2 col = sphereDef.xy - otherSphereDef.xy;
        
        float len_squarred = dot(col, col);
        float radius_squarred = (sphereDef.z+otherSphereDef.z)*(sphereDef.z+otherSphereDef.z);

        if(len_squarred <= radius_squarred)
        {
            colided = had_colided + 1.0;
                
            float push = dot(velocity.xy - otherVelocity.xy, col)/len_squarred;
            float elastic_factor = 2.0 * push * otherSphereDef.w / (otherSphereDef.w + sphereDef.w);
            velocity.xy -= mix(elastic_factor * col, vec2(0.0,0.0), min(had_colided,1.0)); 
        }
    }
    
    if(ifragCoord.x == 0)
        fragColor = colided == 0.0 ? futureDef : (sphereDef + vec4(velocity.xy * frame_delta, 0, 0));
        
    if(ifragCoord.x == 1)
    {
        // Apply gravity pull
        vec2 delta = iMouse.xy - sphereDef.xy;
        velocity.xy += clamp(iMouse.z, 0.0, 1.0) * delta * iTimeDelta * 100000.0 / (length(delta) * length(delta));
        
        float time_left = velocity.w - frame_delta;
        float col_effect = colided > 0.0 ? 0.5 : time_left;

        fragColor = vec4(velocity.xy, colided, col_effect);
    }
        
    if(ifragCoord.x == 2)
        fragColor = vec4(offsetx / float(iResolution.x), offsety / float(iResolution.x), 1, 1);
}
```

# Image

Mettre `Buffer A` dans `iChannel0`.

```glsl
vec4 loadValue( in ivec2 re )
{
    return texelFetch( iChannel0, re, 0 );
}

vec3 drawSphere(in vec2 fragCoord)
{
    if(iMouse.z > 0.0)
    {
        vec2 dist = fragCoord - iMouse.xy;
        float len = dot(dist, dist);
        if(len < (25.0*25.0))
        {
            return vec3(1,1,1);
        }
    }

    for(int i = 0; i < sphereCount; ++i)
    {
        vec4 sphereDef = loadValue(txSpheres + ivec2(0, i));
        
        vec2 dist = fragCoord - sphereDef.xy;
        float len = dot(dist, dist);
        
        if(len < (sphereDef.z*sphereDef.z))
        {
            return loadValue(txSphereColors + ivec2(0, i)).xyz;
        }
    }
    return vec3(0,0,0);
}

vec3 drawEffects(in vec2 fragCoord, in vec3 color)
{
    if(color != vec3(0,0,0))
        return color;

    for(int i = 0; i < sphereCount; ++i)
    {
        vec4 sphereDef = loadValue(txSpheres + ivec2(0, i));
        vec4 sphereVelocityDef = loadValue(txSphereVelocities + ivec2(0, i));
        if(sphereVelocityDef.w <= 0.0)
            continue;
                
        vec2 dist = fragCoord - sphereDef.xy;
        float len = dot(dist, dist);
        float effect_size = sphereDef.z * 1.2;
        
        if(len > (sphereDef.z*sphereDef.z) && len <= (effect_size*effect_size))
        {
            return vec3(sphereVelocityDef.w*2.0);
        }
    }
    return color;
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{   
    // Output to screen
    fragColor = vec4(drawEffects(fragCoord, drawSphere(fragCoord)), 1.0);
}
```
