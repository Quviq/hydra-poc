"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[822],{3905:function(e,t,n){n.d(t,{Zo:function(){return l},kt:function(){return d}});var r=n(7294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function c(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},o=Object.keys(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var s=r.createContext({}),p=function(e){var t=r.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},l=function(e){var t=p(e.components);return r.createElement(s.Provider,{value:t},e.children)},u={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},m=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,o=e.originalType,s=e.parentName,l=c(e,["components","mdxType","originalType","parentName"]),m=p(n),d=a,f=m["".concat(s,".").concat(d)]||m[d]||u[d]||o;return n?r.createElement(f,i(i({ref:t},l),{},{components:n})):r.createElement(f,i({ref:t},l))}));function d(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var o=n.length,i=new Array(o);i[0]=m;var c={};for(var s in t)hasOwnProperty.call(t,s)&&(c[s]=t[s]);c.originalType=e,c.mdxType="string"==typeof e?e:a,i[1]=c;for(var p=2;p<o;p++)i[p]=n[p];return r.createElement.apply(null,i)}return r.createElement.apply(null,n)}m.displayName="MDXCreateElement"},1371:function(e,t,n){n.r(t),n.d(t,{assets:function(){return l},contentTitle:function(){return s},default:function(){return d},frontMatter:function(){return c},metadata:function(){return p},toc:function(){return u}});var r=n(7462),a=n(3366),o=(n(7294),n(3905)),i=["components"],c={slug:7,title:"7. Use with-pattern based component interfaces\n",authors:[],tags:["Accepted"]},s=void 0,p={permalink:"/head-protocol/adr/7",source:"@site/adr/2021-06-11_007-with-pattern-component-interfaces.md",title:"7. Use with-pattern based component interfaces\n",description:"Status",date:"2021-06-11T00:00:00.000Z",formattedDate:"June 11, 2021",tags:[{label:"Accepted",permalink:"/head-protocol/adr/tags/accepted"}],readingTime:1.17,truncated:!1,authors:[],frontMatter:{slug:"7",title:"7. Use with-pattern based component interfaces\n",authors:[],tags:["Accepted"]},prevItem:{title:"6. Network Broadcasts all messages\n",permalink:"/head-protocol/adr/6"},nextItem:{title:"8. Custom Prelude\n",permalink:"/head-protocol/adr/8"}},l={authorsImageUrls:[]},u=[{value:"Status",id:"status",level:2},{value:"Context",id:"context",level:2},{value:"Decision",id:"decision",level:2},{value:"Consequences",id:"consequences",level:2}],m={toc:u};function d(e){var t=e.components,n=(0,a.Z)(e,i);return(0,o.kt)("wrapper",(0,r.Z)({},m,n,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h2",{id:"status"},"Status"),(0,o.kt)("p",null,"Accepted"),(0,o.kt)("h2",{id:"context"},"Context"),(0,o.kt)("p",null,"The ",(0,o.kt)("em",{parentName:"p"},"with pattern")," or ",(0,o.kt)("em",{parentName:"p"},"bracket pattern")," is a functional programming idiom, a\nparticular instance of ",(0,o.kt)("em",{parentName:"p"},"Continuation-Passing Style"),', whereby one component that\ncontrols some resource that is consumed by another component of the system, is\ncreated via a function that takes as argument a function consuming the resource,\ninstead of returning it. This pattern allows safe reclaiming of resources when\nthe "wrapped" action terminates, whether normally or unexpectedly.'),(0,o.kt)("p",null,'TODO "Tying the knot"'),(0,o.kt)("h2",{id:"decision"},"Decision"),(0,o.kt)("p",null,"We use this pattern to provide interfaces to all ",(0,o.kt)("em",{parentName:"p"},"active components"),", which\nexchange messages with other components of the system. A prototypical signature\nof such a component could be:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-hs"},"type Component m = inmsg -> m ()\ntype Callback m = outmsg -> m ()\n\nwithXXX :: Callback m -> (Component m -> m a) -> m a\n")),(0,o.kt)("p",null,"Note that ",(0,o.kt)("inlineCode",{parentName:"p"},"withXXX")," can also allocate resources in order to provide ",(0,o.kt)("inlineCode",{parentName:"p"},"Component"),"\nor use the ",(0,o.kt)("inlineCode",{parentName:"p"},"Callback"),", e.g. fork threads which invoke ",(0,o.kt)("inlineCode",{parentName:"p"},"Callback"),", but also make\nsure they are cleaned up."),(0,o.kt)("h2",{id:"consequences"},"Consequences"),(0,o.kt)("p",null,'Components can be layered on top of another to provide additional behavior given the same interface. This also similar to "decorating" in the object-orientation world.'),(0,o.kt)("p",null,"If the ",(0,o.kt)("inlineCode",{parentName:"p"},"Component")," is agnostic about the messages it consumes/produces, it can be defined as a ",(0,o.kt)("a",{parentName:"p",href:"https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Functor-Contravariant.html"},(0,o.kt)("inlineCode",{parentName:"a"},"Contravariant")," functor")," and the ",(0,o.kt)("inlineCode",{parentName:"p"},"Callback")," part as a (covariant) ",(0,o.kt)("inlineCode",{parentName:"p"},"Functor"),". This makes it possible to use ",(0,o.kt)("inlineCode",{parentName:"p"},"map")," and ",(0,o.kt)("inlineCode",{parentName:"p"},"contramap")," operations to transform messages."))}d.isMDXComponent=!0}}]);