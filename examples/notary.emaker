// taken from "E in a Walnut" by Marc Stiegler 2000
// http://wiki.erights.org/wiki/Walnut/Secure_Distributed_Computing/Capability_Patterns#Vouching_with_Notary.2FInspector

# E sample
####### Widget Inc. software #####

#vouching system 
#returns a private notary that offers a public inspector
#throws problem if the object being vouched is not vouchable
def makeNotary()  {
    def nonObject {}
    def unvouchedException(obj) {throw(`Object not vouchable: $obj`)}
    var vouchableObject := nonObject
    def inspector {
        to vouch(obj) {
            vouchableObject := nonObject
            try {
                obj.startVouch()
                if (vouchableObject == nonObject) {
                    return unvouchedException(obj)
                } else {
                    def vouchedObject := vouchableObject
                    vouchableObject := nonObject
                    return vouchedObject
                }
            } catch err {unvouchedException(obj)}
        }
    }
    def notary {
        to startVouch(obj) { vouchableObject := obj}
        to getInspector()  {return inspector}
    }
    return notary
}

#create Widget Inc's notary
def widgetNotary := makeNotary()

#Order form maker
def makeOrderForm(salesPerson) {
    def orderForm {
        # .... methods for implementing orderForm
        to startVouch() {widgetNotary.startVouch(orderForm)}
    }
    return orderForm
}

#publicly available inspector object 
#(accessible through a uri posted on Widget Inc's web site)
def WidgetInspectionService {
    to getInspector() {return widgetNotary.getInspector()}
}

##### bob software #####

#scaffold for sample
def getOrderFormFromBob()  {return makeOrderForm("scaffold")}

########## Alice's software to vouch for the order form she received from Bob #####

def untrustedOrderForm := getOrderFormFromBob()
def inspector := WidgetInspectionService.getInspector()
def trustedOrderForm := inspector.vouch(untrustedOrderForm)
 