const n=7n,e=6n,t=e+1n,o=(1n<<n+n*e)-1n^((1n<<t*n)-1n)/((1n<<t)-1n)<<e,r=[1n,7n,6n,8n],c=[0n,0n],i=Array.from({length:Number(n)},((e,t)=>BigInt(t)*n));let d=0;function s(){return(c[0]|c[1])===o}function l(){const n=c[1-d];for(const e of r){const t=n&n>>e;if(t&t>>2n*e)return!0}}function u(){for(const n of g()){if(a(n),l())return f(n);f(n)}return!0}function a(n){c[d]^=1n<<i[n]++,d=1-d}function f(n){d=1-d,c[d]^=1n<<--i[n]}function m(){let n=0;const e=c[1-d];for(const t of r)e&e>>t&&(n+=20),e&e>>t&e>>2n*t&&(n+=50);return n}function*g(){for(let t=0;t<n;t++)i[t]<BigInt(t)*n+e&&(yield t)}const y=Number(e),E=Number(n),I=["red","yellow"],L=document.getElementById("board");L.tBodies[0].innerHTML=`<tr>${"<td></td>".repeat(E)}</tr>`.repeat(y);const h=document.getElementById("turn"),B=document.getElementById("start"),p=document.getElementById("result"),v=[()=>new Promise((n=>L.addEventListener("click",(function e(t){const o=t.target.cellIndex;void 0!==o&&i[o]<y+o*E&&(L.removeEventListener("click",e),n(o))})))),async()=>function(){let n,e=-1;for(const t of g()){if(a(t),l())return f(t),t;if(u()){const o=m();o>e&&(n=t,e=o)}else void 0===n&&(n=t);f(t)}return n}(),async()=>{const n=[...g()];return n[Math.floor(Math.random()*n.length)]}];function x(n){return v[document.getElementById(`player-${n}`).selectedIndex]}B.addEventListener("click",(async()=>{B.classList.replace("px-7","px-5"),B.textContent="Recommencer",B.addEventListener("click",(()=>location.reload())),h.hidden=!1,p.hidden=!1;const n=[x(1),x(2)];for(;!s()&&!l();){const e=await n[d]();L.rows[y-1-Number(i[e])+e*E].cells[e].classList.add(I[d]),a(e),h.classList.replace(I[1-d],I[d])}h.hidden=!0,p.textContent=s()?"Partie nulle !":`Le joueur ${2-d} a gagné !`,p.hidden=!1}),{once:!0});
