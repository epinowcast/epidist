tar_map(
  values = list(model_name = machine_model_names, model = models),
  names = model_name,
  tar_target(
    standata,
    do.call(
      model,
      list(data = list_observations[[1]], fn = brms::make_standata)
    ),
    pattern = map(list_observations)
  )
)
