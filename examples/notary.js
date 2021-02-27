// taken from "E in a Walnut" by Marc Stiegler 2000
// http://wiki.erights.org/wiki/Walnut/Secure_Distributed_Computing/Capability_Patterns#Vouching_with_Notary.2FInspector

import harden from '@agoric/harden';

// E sample
// ##### Widget Inc. software #####

// vouching system
// returns a private notary that offers a public inspector
// throws problem if the object being vouched is not vouchable
const Notary = harden({ make()  {
    const nonObject = {};
    function unvouchedException(obj) { throw(`Object not vouchable: ${obj}`); }
    let vouchableObject = nonObject;
    const inspector = harden({
        vouch(obj) {
            vouchableObject = nonObject;
            try {
                obj.startVouch();
                if (vouchableObject === nonObject) {
                    return unvouchedException(obj);
                } else {
                    const vouchedObject = vouchableObject;
                    vouchableObject = nonObject;
                    return vouchedObject;
                }
            } catch(err) { return unvouchedException(obj); }
        }
    });
    const notary = harden({
        startVouch(obj) { vouchableObject = obj; },
        getInspector()  { return inspector; }
    });
    return notary;
} });

// create Widget Inc's notary
const widgetNotary = Notary.make();

// Order form maker
function makeOrderForm(salesPerson) {
    const orderForm = harden({
        // .... methods for implementing orderForm
        startVouch() { widgetNotary.startVouch(orderForm); }
    });
    return orderForm;
}

// publicly available inspector object
// (accessible through a uri posted on Widget Inc's web site)
const WidgetInspectionService = harden({
    getInspector() { return widgetNotary.getInspector(); }
});

// ##### bob software #####

//  scaffold for sample
function getOrderFormFromBob()  { return makeOrderForm("scaffold"); }

// ########## Alice's software to vouch for the order form she received from Bob #####

const untrustedOrderForm = getOrderFormFromBob();
const inspector = WidgetInspectionService.getInspector();
const trustedOrderForm = inspector.vouch(untrustedOrderForm);
