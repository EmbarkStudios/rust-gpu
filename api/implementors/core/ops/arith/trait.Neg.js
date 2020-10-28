(function() {var implementors = {};
implementors["nix"] = [{"text":"impl Neg for TimeSpec","synthetic":false,"types":[]},{"text":"impl Neg for TimeVal","synthetic":false,"types":[]}];
implementors["ordered_float"] = [{"text":"impl&lt;T:&nbsp;Float&gt; Neg for OrderedFloat&lt;T&gt;","synthetic":false,"types":[]},{"text":"impl&lt;T:&nbsp;Float&gt; Neg for NotNan&lt;T&gt;","synthetic":false,"types":[]}];
implementors["spirv_std"] = [{"text":"impl Neg for Vec2","synthetic":false,"types":[]},{"text":"impl Neg for Vec3","synthetic":false,"types":[]},{"text":"impl Neg for Vec4","synthetic":false,"types":[]}];
implementors["time"] = [{"text":"impl Neg for Duration","synthetic":false,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()