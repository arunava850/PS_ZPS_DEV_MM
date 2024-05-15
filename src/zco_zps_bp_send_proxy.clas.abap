class ZCO_ZPS_BP_SEND_PROXY definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !DESTINATION type ref to IF_PROXY_DESTINATION optional
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    preferred parameter LOGICAL_PORT_NAME
    raising
      CX_AI_SYSTEM_FAULT .
  methods SEND_BP
    importing
      !OUTPUT type ZMT_BP_SEND_PROXY
    raising
      CX_AI_SYSTEM_FAULT .
protected section.
private section.
ENDCLASS.



CLASS ZCO_ZPS_BP_SEND_PROXY IMPLEMENTATION.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZCO_ZPS_BP_SEND_PROXY'
    logical_port_name   = logical_port_name
    destination         = destination
  ).

  endmethod.


  method SEND_BP.

  data(lt_parmbind) = value abap_parmbind_tab(
    ( name = 'OUTPUT' kind = '0' value = ref #( OUTPUT ) )
  ).
  if_proxy_client~execute(
    exporting
      method_name = 'SEND_BP'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.
ENDCLASS.
